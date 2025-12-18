(in-package #:hunchentoot-recycling-taskmaster-test)

;;; Use definitions in 'shutdown.lisp'

(1am:test abandon-with-no-accept
  (with-making-test-server (server)
    (1am:signals usocket:address-in-use-error
      (with-making-test-server (server2)))
    (1am:is (abandon-acceptor server))
    ;; Checks the address is released.
    (1am:is
     (with-making-test-server (server2)
       t))
    (setf server nil)))
