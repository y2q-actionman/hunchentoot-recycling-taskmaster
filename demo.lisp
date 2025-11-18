(in-package :cl-user)

(ql:quickload "hunchentoot-recycle")

;;; Defines a test page, from the doc of Hunchentoot

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

;;; Start

(defparameter *test-server*
  (let* ((taskmaster
      	   (make-instance 'hunchentoot-recycle:recycling-taskmaster))
         (acceptor (make-instance 'hunchentoot-recycle:parallel-easy-acceptor
                                  :port 4242
                                  :taskmaster taskmaster)))
    (hunchentoot:start acceptor)))

; See "http://127.0.0.1:4242/yo" or "http://127.0.0.1:4242/yo?name=Dude" .


;;; Stop

(hunchentoot:stop *test-server*)
