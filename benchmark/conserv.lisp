(in-package :hunchentoot-recycling-taskmaster-benchmark)

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
