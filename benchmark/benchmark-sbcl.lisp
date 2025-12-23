(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; Wookie

;; (defun bench-wookie-static ()
;;   (let ((server-thread
;;           (bt:make-thread
;;            #'wookie-helper:start-static-server ; I found it in wookie-helper package.
;;            :name "Wookie server thread")))
;;     (wait-for-starting-server)
;;     (unwind-protect
;;          (run-wrk "http://localhost:8080" "wookie_static.log")
;;       (bt:destroy-thread server-thread))
;;     server-thread))

(wookie:defroute (:get "/hello-world") (req res)
  (declare (ignore req))
  ;; see `handler-small-sleep'
  (flet ((thunk ()
           (wookie:send-response res :body "Hello, World!")))
    (if (plusp *handler-sleep-seconds*)
        (cl-async:delay #'thunk
          :time *handler-sleep-seconds*)
        (thunk))))

(defun start-wookie-server ()     ; from wookie-doc/views/doc/index.md
  (let (listener server)
    (unwind-protect
         (as:with-event-loop ()
           (setf listener (make-instance 'wookie:listener :bind nil :port 8080)
                 server (wookie:start-server listener)))
      (as:close-tcp-server server))))

;; FIXME: This definition seems not supporting "Connection: close".
(defun bench-wookie ()
  (let ((server-thread
          (bt:make-thread
           (lambda () (start-wookie-server)) 
           :name "Wookie server thread")))
    (wait-for-starting-server)
    (unwind-protect
         (progn
           ;; (uiop:run-program "curl -v http://localhost:8080/hello-world"
           ;;                   :output :interactive)
           (run-wrk "http://localhost:8080/hello-world" "wookie_default.log"))
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

;;; Conserv
;;; 
;;; FIXME: I currently gave up test it.
;;; 
;;; When I run "hello.lisp" included in it as a sample code and run "curl' like below:
;;; 
;;;   curl -v http://localhost:8888
;;;
;;; I got this error on my SBCL:
;;; 
;;; The value "UPGRADE" is not of type SIMPLE-BASE-STRING when binding SB-IMPL::STRING1
;;;    [Condition of type TYPE-ERROR]

;;; From conserv/examples/http/hello.lisp
(defclass hello () ())

(defmethod conserv.http:on-http-request ((driver hello))
  ;; XXX: FIXME: how to sleep here, what is alternative of (handler-small-sleep)
  ;; (conserv.tcp:tcp-listener-pause driver :timeout *handler-sleep-seconds*)
  (conserv.http:set-headers conserv.http:*request* :content-type "text/html")
  (format conserv.http:*request* "<h1>Hello, world!</h1>")
  (close conserv.http:*request*))

(defun conserv-start ()
  (conserv:with-event-loop ()
    (conserv.http:http-listen (make-instance 'hello)
                              :port 8888)))
