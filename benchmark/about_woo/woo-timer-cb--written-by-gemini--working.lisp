;; ファイル名: timer-server.lisp

(defpackage #:timer-server
  (:use #:cl))
(in-package #:timer-server)

 ;;;
 ;;; 0. 必要なライブラリをロード
 ;;;
(ql:quickload '("woo" "clack" "cffi" "clack") :silent t)

 ;;;
 ;;; 1. CFFIコールバック関数の定義
 ;;;
 ;;; これはタイマーが発火したときにCの世界から呼び出される関数です。
 ;;; この関数自体は、どのresponderを呼ぶべきかを知らないため、
 ;;; 登録しておいたLispの関数を呼び出す中継役を果たします。
(cffi:defcallback timer-cb :void ((loop :pointer) (watcher :pointer) (events :int))
  (declare (ignore events))
  (handler-case
      (let ((callback (gethash (cffi:pointer-address watcher)
                               woo.ev.event-loop::*callbacks*)))
        (when callback
          ;; 登録しておいたLispのクロージャを実行
          (funcall callback)))
    (error (e)
      (format *error-output* "Error in timer-cb: ~A~%" e)))
  ;; 後片付け
  (lev:ev-timer-stop loop watcher)
  (remhash (cffi:pointer-address watcher) woo.ev.event-loop::*callbacks*)
  (cffi:foreign-free watcher))

 ;;;
 ;;; 2. Clackアプリケーション（ハンドラ）の定義
 ;;;
(defparameter *app*
  (lambda (env)
    (declare (ignore env))
    ;; 非同期モードに入るため、関数（lambda）を返す
    (lambda (responder)
      (let ((timer (cffi:foreign-alloc '(:struct lev:ev-timer))))
        ;; --- ここからが重要 ---

        ;; 1. Lispの処理（クロージャ）を用意する
        ;;    このクロージャは、後で呼び出すべき「responder」を記憶している
        (let ((lisp-callback
                (lambda ()
                  ;; (format t "~&[Callback] Timer fired. Sending response.~%")
                  (funcall responder
                           '(200 (:content-type "text/plain") ("Hello after 1 second!"))))))

          ;; 2. CのタイマーオブジェクトとLispの処理を紐付ける
          ;;    woo内部の*callbacks*ハッシュテーブルを利用し、
          ;;    タイマーのメモリアドレスをキーにして、Lispクロージャを保存する
          (setf (gethash (cffi:pointer-address timer) woo.ev.event-loop::*callbacks*)
                lisp-callback))

        ;; 3. タイマーを初期化し、1秒後に'timer-cb'を呼び出すよう設定
        (lev:ev-timer-init timer 'timer-cb 1.0d0 0.0d0)
        ;; (lev:ev-timer-init timer 'timer-cb 0.001d0 0.0d0)

        ;; 4. 現在のイベントループでタイマーを開始する
        (lev:ev-timer-start woo.ev.event-loop:*evloop* timer)

        ;; (format t "~&[Handler] Request received. Timer set for 1 second. Worker is now free.~%")
        ))))

 ;;;
 ;;; 3. サーバーの起動と停止
 ;;;
(defvar *server* nil)

(setf woo:*default-worker-num* 8)

(defun start-server (&key (port 5000))
  (when *server*
    (error "Server is already running."))
  (setf *server* (clack:clackup *app* :server :woo :port port))
  (format t "Server started on http://127.0.0.1:~A~%" port))

(defun stop-server ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (format t "Server stopped.~%")))

#|
TIMER-SERVER> (start-server)
NOTICE: Running in debug mode. Debugger will be invoked on errors.
  Specify ':debug nil' to turn it off on remote environments.
Woo server is started.
Listening on 127.0.0.1:5000.
Server started on http://127.0.0.1:5000
NIL


$ curl -v http://localhost:5000/sleep
* Host localhost:5000 was resolved.
* IPv6: ::1
* IPv4: 127.0.0.1
*   Trying [::1]:5000...
* connect to ::1 port 5000 from ::1 port 59842 failed: Connection refused
*   Trying 127.0.0.1:5000...
* Connected to localhost (127.0.0.1) port 5000
> GET /sleep HTTP/1.1
> Host: localhost:5000
> User-Agent: curl/8.5.0
> Accept: */*
>
< HTTP/1.1 200 OK
< Date: Mon, 22 Dec 2025 14:43:54 GMT
< Connection: keep-alive
< Content-Type: text/plain
< Transfer-Encoding: chunked
<
* Connection #0 to host localhost left intact
Hello after 1 second!
|#
