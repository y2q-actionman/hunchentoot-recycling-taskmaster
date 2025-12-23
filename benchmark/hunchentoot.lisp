(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; Hunchentoot and its families.

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (handler-small-sleep)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defun bench-hunchentoot-using-class (acceptor-class taskmaster-class log-file-name asdf-system-name
                                      &key taskmaster-args)
  (assert (subtypep acceptor-class 'hunchentoot:easy-acceptor))
  (let ((server (make-instance acceptor-class
                               :taskmaster (apply #'make-instance taskmaster-class taskmaster-args)
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" log-file-name asdf-system-name)
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot ()
  (bench-hunchentoot-using-class 'hunchentoot:easy-acceptor
                                 'hunchentoot:one-thread-per-connection-taskmaster
                                 "hunchentoot_default.log"
                                 :hunchentoot))
