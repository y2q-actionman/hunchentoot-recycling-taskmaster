(in-package :cl-user)

(ql:quickload "hunchentoot")
(ql:quickload "hunchentoot-recycle")
(ql:quickload "cl-tbnl-gserver-tmgr")
(ql:quickload "quux-hunchentoot")

(ql:quickload "wookie")
;;; sudo apt install libuv1-dev
;;;
;;; [package cl-async-ssl]
;;; Unable to load any of the alternatives:
;;;    ("libcrypto.so.1.1" "libcrypto.so.1.0.2" "libcrypto.so")
;;;    [Condition of type CFFI:LOAD-FOREIGN-LIBRARY-ERROR]
;;;
;;; sudo apt install libssl-dev

(ql:quickload "woo")
;;; sudo apt install libev-dev
;;; https://lisp-journey.gitlab.io/blog/why-turtl-switched-from-lisp-to-js/
;;; https://news.ycombinator.com/item?id=29019217

(ql:quickload "house")

;;; https://killtheradio.net/technology/cl-async-non-blocking-asynchronous-programming-for-common-lisp/

(ql:quickload "clack")

(ql:quickload "conserv")
;; sudo apt install libfixposix-dev

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nproc ()
  (let ((nproc-str
          (with-output-to-string (*standard-output*)
            (uiop:run-program "nproc" :output t))))
    (parse-integer nproc-str)))

(defparameter *nproc* (nproc))

(defparameter *wrk-duration* 10)

(defun run-wrk (host filename)
  (with-open-file (*standard-output* filename :direction :output :if-exists :rename)
    (format t "~A ~A ~A~2%"
            filename  (lisp-implementation-type) (lisp-implementation-version))
    (finish-output)
    (flet ((run-tcd (thread connection keep-alive &optional (duration *wrk-duration*))
             (let ((options (list "-t" (princ-to-string thread)
                                  "-c" (princ-to-string connection)
                                  "-d" (princ-to-string duration)
                                  host)))
               (unless keep-alive
                 (push "Connection: close" options)
                 (push "-H" options))
               (format t "# ~{~A ~^~}~%" options)
               (finish-output)
               (uiop:run-program (list* "wrk" options) :output t))
             (terpri)
             (finish-output)))
      (run-tcd 4 100 t)
      ;; (run-tcd 4 100 nil)
      (run-tcd 4 10 t)
      ;; (run-tcd 4 10 nil)
      (run-tcd 16 400 t)
      ;; (run-tcd 16 400 nil)
      )))

;;; Hunchentoot and variant

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (sleep 0.001)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defun bench-hunchentoot ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_default.log")
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot-atomic-taskmaster ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'hunchentoot-recycle:atomic-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-taskmaster_default.log")
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot-atomic-acceptor ()
  (let ((server (make-instance 'hunchentoot-recycle:atomic-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-acceptor_default.log")
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot-atomic-all ()
  (let ((server (make-instance 'hunchentoot-recycle:atomic-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'hunchentoot-recycle:atomic-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-all_default.log")
      (hunchentoot:stop server :soft t))
    server))

;;; hunchentoot-recycle

(defparameter *hunchentoot-recycle-default-thread-count*
  hunchentoot-recycle::*default-initial-thread-count*)

(defun bench-hunchentoot-recycle (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycle_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycle-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycle:recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycle:parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

(defun bench-hunchentoot-recycle-atomic-all (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycle-atomic-all_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycle-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycle:atomic-recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycle:atomic-parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

(defun bench-hunchentoot-recycle-atomic-acceptor (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycle-atomic-acceptor_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycle-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycle:recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycle:atomic-parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

(defun bench-hunchentoot-recycle-atomic-taskmaster (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycle-atomic-taskmaster_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycle-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycle:atomic-recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycle:parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

;;; cl-tbnl-gserver-tmgr

(defparameter *cl-tbnl-gserver-tmgr-default-thread-count*
  cl-tbnl-gserver-tmgr.tmgr::*gserver-tmgr-poolsize*)

(defun bench-cl-tbnl-gserver-tmgr (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "cl-tbnl-gserver-tmgr_threads-~D~@[-default~*~].log"
                         threads (eql threads *cl-tbnl-gserver-tmgr-default-thread-count*))
    as server = (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'cl-tbnl-gserver-tmgr.tmgr:gserver-tmgr
                                                          :max-thread-count threads))
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

;;; quux-hunchentoot

(defun bench-quux-hunchentoot ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'quux-hunchentoot:thread-pooling-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "quux-hunchentoot_default.log")
      (hunchentoot:stop server :soft t))
    server))

;;; Wookie

(defun bench-wookie ()
  (let ((server-thread
          (bt:make-thread
           #'wookie-helper:start-static-server ; I found it in wookie-helper package.
           :name "Wookie server thread")))
    (sleep 1)                           ; wait for starting
    (unwind-protect
         (run-wrk "http://localhost:8080" "wookie_default.log")
      (bt:destroy-thread server-thread))
    server-thread))

;;; Woo

(defun bench-woo (&optional (threads-list '(nil)))
  (loop
    for threads in threads-list
    as logname = (format nil "woo_threads-~D~@[-default~*~].log"
                         threads (eql threads nil))
    as thunk = (lambda ()
                 ;; from README
                 (woo:run
                  (lambda (env)
                    (declare (ignore env))
                    '(200 (:content-type "text/plain") ("Hello, World")))
                  :worker-num threads))
    as server-thread = (bt:make-thread
                        thunk :name (format nil "Woo server thread ~A" threads))
    collect server-thread
    do (sleep 1)                           ; wait for starting
       (unwind-protect
            (run-wrk "http://localhost:5000" logname)
         (bt:destroy-thread server-thread))))

(defparameter *woo-callback-app*
  (lambda (_env)
    (declare (ignore _env))
    (lambda (callback)
      (describe callback)
      (describe woo.ev:*evloop*)
      
      ;; (funcall callback '(200 (:content-type "text/plain") ("Hello, World")))
      
      (let ((evloop woo.ev:*evloop*))
        (bt:make-thread (lambda (&aux (woo.ev:*evloop* 0))
                          (describe evloop)
                          (describe woo.ev:*evloop*)
                          (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))

      (princ "end!" *standard-output*))))

(defun bench-woo-callback (&optional (threads-list '(nil)))
  (loop
    for threads in threads-list
    as logname = (format nil "woo-callback_threads-~D~@[-default~*~].log"
                         threads (eql threads nil))
    as thunk = (lambda ()
                 (woo:run
                  *woo-callback-app*
                  :worker-num threads))
    as server-thread = (bt:make-thread
                        thunk :name (format nil "Woo server thread ~A" threads))
    collect server-thread
    do (sleep 1)                           ; wait for starting
       (unwind-protect
            (run-wrk "http://localhost:5000" logname)
         (bt:destroy-thread server-thread))))

;;; ????
;;; https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/Lisp/woo/woo.ros
;; https://www.reddit.com/r/Common_Lisp/comments/1hmxv4k/comment/m4feb4b/

;; (defparameter *woo-callback-threads-app*
;;   (lambda (_env)
;;     (declare (ignore _env))
;;     (lambda (callback)
;;       (bt:make-thread (lambda ()
;;                         (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))))

;; (defun bench-woo-callback-threads (&optional (threads-list '(nil)))
;;   (loop
;;     for threads in threads-list
;;     as logname = (format nil "woo-callback_threads-~D~@[-default~*~].log"
;;                          threads (eql threads nil))
;;     as server = (clack:clackup *woo-callback-threads-app* :server :woo)
;;     do (sleep 1)                        ; wait for starting
;;        (unwind-protect
;;             (run-wrk "http://localhost:5000" logname)
;;          (clack:stop server))))

;;; House

(house:define-handler (hello-world :content-type "text/plain") ()
  "Hello world!")

(defun bench-house ()
  (let ((server-thread
          (bt:make-thread
           (lambda () (house:start 4040))
           :name "House server thread")))
    (sleep 1)                           ; wait for starting
    (unwind-protect
         (run-wrk "http://localhost:4040" "house_default.log")
      (bt:destroy-thread server-thread))
    server-thread))

;;; Conserv

(defclass hello () ())

(defmethod conserv.http:on-http-request ((driver hello))
  (conserv.http:set-headers conserv.http:*request* :content-type "text/html")
  (format conserv.http:*request* "<h1>Hello, world!</h1>")
  (close conserv.http:*request*))

(defun conserv-start ()
  (conserv:with-event-loop ()
  (conserv.http:http-listen (make-instance 'hello)
                            :port 8888)))

;; (defun bench-house ()
;; (let ((server-thread
;;         (bt:make-thread
;;          (lambda () (house:start 4040))
;;          :name "House server thread")))
;;   (sleep 1)                             ; wait for starting
;;   (unwind-protect
;;        (run-wrk "http://localhost:4040" "house_default.log")
;;     (bt:destroy-thread server-thread))
;;   server-thread))

;;; for repl
(trace run-wrk)
(defun bench-all ()
  (bench-hunchentoot)
  (bench-hunchentoot-atomic-taskmaster)
  (bench-hunchentoot-atomic-acceptor)
  (bench-hunchentoot-atomic-all)
  (bench-hunchentoot-recycle (list 8 *nproc*))
  (bench-hunchentoot-recycle-atomic-all (list 8 *nproc*))
  (bench-hunchentoot-recycle-atomic-acceptor (list 8 *nproc*))
  (bench-hunchentoot-recycle-atomic-taskmaster (list 8 *nproc*))
  (bench-cl-tbnl-gserver-tmgr (list 8 *nproc*))
  (bench-quux-hunchentoot)
  (bench-wookie)
  (bench-woo (list nil 8 *nproc*))
  (bench-woo-callback (list nil 8 *nproc*))
  (bench-house))
