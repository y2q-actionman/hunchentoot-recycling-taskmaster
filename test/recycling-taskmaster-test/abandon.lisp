(in-package #:hunchentoot-recycling-taskmaster-test)

;;; Use definitions in 'shutdown.lisp'

(defun check-address-port-usable ()
  "Checks the localhost address and port is released."
  (with-making-test-server (server2)
    t))

(1am:test abandon-with-no-accept
  (with-making-test-server (server)
    (1am:signals usocket:address-in-use-error
      (check-address-port-usable))
    (1am:is (abandon-acceptor server))
    (1am:is (check-address-port-usable))
    (setf server nil)))
