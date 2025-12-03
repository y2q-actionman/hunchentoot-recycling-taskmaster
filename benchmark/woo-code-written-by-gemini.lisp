;; ファイル名: async-sample.lisp

(in-package #:cl-user)

;;;
;;; 0. 必要なライブラリをロードする
;;; (事前にQuicklispでインストールしておく必要があります)
;;; (ql:quickload '("woo" "clack" "lparallel"))
;;;
(ql:quickload '("woo" "clack" "lparallel") :silent t)


;;;
;;; 1. lparallelのセットアップ
;;;
;;; アプリケーションの起動時に一度だけ、ワーカースレッドのプールを作成します。
;;; ここではCPUのコア数と同じ数のスレッドを作成しています。
(defun setup-lparallel ()
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel
                              8
                              :name "async-worker-pool"))))

;;;
;;; 2. 時間のかかる処理を模した関数
;;;
(defun heavy-task (seconds)
  (format t "~&[Worker Thread] Starting heavy task (will sleep for ~A seconds)...~%" seconds)
  (sleep seconds)
  (format t "~&[Worker Thread] Finished heavy task.~%")
  (format nil "Slept for ~A seconds." seconds))


;;;
;;; 3. Clackのアプリケーション（ハンドラ）
;;;
(defvar *app*
  (lambda (env)
    (let ((path (getf env :path-info)))
      (cond
        ((string= path "/")
         '(200 (:content-type "text/plain") ("Hello, World!")))

        ((string= path "/sleep")
         (format t "[HTTP Thread] Received request for /sleep. Offloading to worker pool.~%")
         (lambda (responder)
           (lparallel:submit-task
            #'(lambda (dummy-arg)
                (declare (ignore dummy-arg))
                (let* ((result (heavy-task 5))
                       (response `(200 (:content-type "text/plain") (,result))))
                  (format t "[Worker Thread] Sending response via responder.~%")
                  (funcall responder response)))
            nil)))   ; ★修正★ submit-taskとlambda(responder)を閉じる
        (t
         '(404 (:content-type "text/plain") ("Not Found"))))))) ; ★修正★ cond, let, lambda, defvarを閉じる( *app*


;;;
;;; 4. サーバーの起動と停止
;;;
(defvar *server* nil)

(defun start-server (&key (port 5000))
  (when *server*
    (error "Server is already running."))
  (setup-lparallel)
  (setf *server* (clack:clackup *app* :server :woo :port port))
  (format t "Server started on http://127.0.0.1:~A~%" port))

(defun stop-server ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (format t "Server stopped.~%"))
  (when lparallel:*kernel*
    (lparallel:end-kernel :wait t)
    (setf lparallel:*kernel* nil)
    (format t "lparallel kernel stopped.~%")))

;; REPLから (start-server) で起動
;; (stop-server) で停止
