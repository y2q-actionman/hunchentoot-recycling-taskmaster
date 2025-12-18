(in-package #:hunchentoot-recycling-taskmaster-test)

(defparameter *test-acceptor-class*
  'hunchentoot-recycling-taskmaster:parallel-easy-acceptor)

(defparameter *test-port* 64241)

(defun run-hunchentoot-tests ()
  "Copied from 'hunchentoot/run-test.lisp'"
  (format t "~&;; Starting web server on localhost:~A." *test-port*)
  (force-output)
  (let ((server (hunchentoot:start (make-instance *test-acceptor-class* :port *test-port*))))
    (unwind-protect
         (progn
           (format t "~&;; Sleeping 2 seconds to give the server some time to start...")
           (force-output)
           (sleep 2)
           (format t "~&;; Now running confidence tests.")
           (force-output)
           (hunchentoot-test:test-hunchentoot (format nil "http://localhost:~A" *test-port*)))
      (format t "~&;; Stopping server.")
      (force-output)
      (hunchentoot:stop server)
      (format t "~&;; Cleaning temporary files.")
      (hunchentoot-test::clean-tmp-dir))))

(1am:test hunchentoot-tests
  (run-hunchentoot-tests))
