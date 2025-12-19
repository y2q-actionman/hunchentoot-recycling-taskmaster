(in-package #:hunchentoot-recycling-taskmaster-test)

(defparameter *test-port* 64241)

(defun run-hunchentoot-tests
    (&key (acceptor-class 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor)
       (taskmaster-class 'hunchentoot-recycling-taskmaster:recycling-taskmaster))
  "Copied from 'hunchentoot/run-test.lisp'"
  (assert (subtypep acceptor-class 'hunchentoot:easy-acceptor))
  (format t "~&;; Starting web server on localhost:~A." *test-port*)
  (force-output)
  (let ((server (hunchentoot:start
                 (make-instance acceptor-class
                                :port *test-port*
                                :taskmaster (make-instance taskmaster-class)))))
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
      (hunchentoot-test::clean-tmp-dir)))
  t)

(1am:test hunchentoot-tests
  (1am:is (run-hunchentoot-tests)))
