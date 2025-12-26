(in-package :cl-user)

(ql:quickload "hunchentoot-recycling-taskmaster")

;;; Defines a test page, from the doc of Hunchentoot
(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

;;; Start
(defparameter *test-server*
  (make-instance 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
                 :port 4242))
(hunchentoot:start *test-server*)

;;; See "http://127.0.0.1:4242/yo" or "http://127.0.0.1:4242/yo?name=Dude".
;;; 
;;; $ curl "http://127.0.0.1:4242/yo"
;;; Hey!

;;; Stop
(hunchentoot:stop *test-server*)
