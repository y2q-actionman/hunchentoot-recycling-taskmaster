(in-package :cl-user)

(ql:quickload "hunchentoot")
(ql:quickload "hunchentoot-recycling-taskmaster")
(ql:quickload "hunchentoot-atomic-op-taskmaster")
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
;;; ????
;;; https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/Lisp/woo/woo.ros
;; https://www.reddit.com/r/Common_Lisp/comments/1hmxv4k/comment/m4feb4b/


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
(defparameter *test-keep-alive* t)
(defparameter *test-no-keep-alive* t)

(defconstant +sleep-seconds+ 0)        ; or 0.001. TODO: add Wookie support.

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
      (loop for (threads connections) in '((4 100) (4 10) (16 400))
            if *test-keep-alive*
              do (run-tcd threads connections t)
            if *test-no-keep-alive*
              do (run-tcd threads connections nil)))))

;;; Hunchentoot

(defun handler-small-sleep ()
  (when (plusp +sleep-seconds+)
    (sleep +sleep-seconds+)))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (handler-small-sleep)
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

;;; hunchentoot-atomic-op-taskmaster

(defun bench-hunchentoot-atomic-taskmaster ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-taskmaster_default.log")
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot-atomic-acceptor ()
  (let ((server (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-acceptor_default.log")
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot-atomic-all ()
  (let ((server (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-all_default.log")
      (hunchentoot:stop server :soft t))
    server))

;;; hunchentoot-recycling-taskmaster

(defparameter *hunchentoot-recycling-taskmaster-default-thread-count*
  hunchentoot-recycling-taskmaster::*default-initial-thread-count*)

(defun bench-hunchentoot-recycling-taskmaster (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycling-taskmaster:recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

;;; hunchentoot-recycling-taskmaster + atomic-op

(defun bench-hunchentoot-recycling-taskmaster-atomic-all (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster-atomic-all_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

(defun bench-hunchentoot-recycling-taskmaster-atomic-acceptor (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster-atomic-acceptor_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycling-taskmaster:recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

(defun bench-hunchentoot-recycling-taskmaster-atomic-taskmaster (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster-atomic-taskmaster_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
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
;;; TODO: take +sleep-seconds+

(defun wait-for-starting-server ()
  (sleep 1))

(defun bench-wookie ()
  (let ((server-thread
          (bt:make-thread
           #'wookie-helper:start-static-server ; I found it in wookie-helper package.
           :name "Wookie server thread")))
    (wait-for-starting-server)
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
                    (handler-small-sleep)
                    '(200 (:content-type "text/plain") ("Hello, World")))
                  :worker-num threads))
    as server-thread = (bt:make-thread
                        thunk :name (format nil "Woo server thread ~A" threads))
    collect server-thread
    do (wait-for-starting-server)
       (unwind-protect
            (run-wrk "http://localhost:5000" logname)
         (bt:destroy-thread server-thread))))

(defparameter *woo-callback-app*
  (lambda (_env)
    (declare (ignore _env))
    (lambda (callback)
      ;; This is works, but no meaning to sleep.
      (handler-small-sleep)
      (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))

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
    do (wait-for-starting-server)
       (unwind-protect
            (run-wrk "http://localhost:5000" logname)
         (bt:destroy-thread server-thread))))

      
;; Making a thread like below don't works because `*EVLOOP*' is NIL.
;; (defparameter *woo-callback-threads-app*
;;   (lambda (_env)
;;     (declare (ignore _env))
;;     (lambda (callback)
;;       (bt:make-thread (lambda ()
;;                         (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))))

;; Binding like that does not works also.
;; (let ((evloop woo.ev:*evloop*))
;;   (bt:make-thread (lambda (&aux (woo.ev:*evloop* evloop))
;;                     (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))

;; (defun bench-woo-callback-threads (&optional (threads-list '(nil)))
;;   (loop
;;     for threads in threads-list
;;     as logname = (format nil "woo-callback_threads-~D~@[-default~*~].log"
;;                          threads (eql threads nil))
;;     as server = (clack:clackup *woo-callback-threads-app* :server :woo)
;;     do (wait-for-starting-server)
;;        (unwind-protect
;;             (run-wrk "http://localhost:5000" logname)
;;          (clack:stop server))))

;;; House

(house:define-handler (hello-world :content-type "text/plain") ()
  (handler-small-sleep)
  "Hello world!")

(defun bench-house ()
  (let ((server-thread
          (bt:make-thread
           (lambda () (house:start 4040))
           :name "House server thread")))
    (wait-for-starting-server)
    (unwind-protect
         (run-wrk "http://localhost:4040" "house_default.log")
      (bt:destroy-thread server-thread))
    server-thread))

;;; Conserv

(defclass hello () ())

(defmethod conserv.http:on-http-request ((driver hello))
  (handler-small-sleep)
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
;;   (wait-for-starting-server)
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
  (bench-hunchentoot-recycling-taskmaster (list 8 *nproc*))
  (bench-hunchentoot-recycling-taskmaster-atomic-all (list 8 *nproc*))
  (bench-hunchentoot-recycling-taskmaster-atomic-acceptor (list 8 *nproc*))
  (bench-hunchentoot-recycling-taskmaster-atomic-taskmaster (list 8 *nproc*))
  (bench-cl-tbnl-gserver-tmgr (list 8 *nproc*))
  (bench-quux-hunchentoot)
  (bench-wookie)
  (bench-woo (list nil 8 *nproc*))
  (bench-woo-callback (list nil 8 *nproc*))
  (bench-house))
