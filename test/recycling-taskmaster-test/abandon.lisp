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

(1am:test abandon-after-one-accept
  (with-making-test-server (server)
    (1am:is (drakma:http-request *test-server-url*))
    (1am:is (abandon-acceptor (shiftf server nil)))
    (1am:is (check-address-port-usable))))

;;; Worker threads are somewhere.

(1am:test abandon-at-before-handle-incoming-connection
  (with-making-test-server (server)
    (setf (before-handle-incoming-connection-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (ignore-errors
                            (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-before-handle-incoming-connection)
      (schedule-waking-up-a-worker-in-before-handle-incoming-connection)
      (1am:is (abandon-acceptor (shiftf server nil)))
      (1am:is (check-address-port-usable))
      (1am:is (join-thread client-thread)))))

(1am:test abandon-in-hello-world-handler
  (with-making-test-server (server)
    (setf (hello-world-handler-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (ignore-errors
                            (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-hello-world-handler)
      (schedule-waking-up-a-worker-in-hello-world-handler)
      (1am:is (abandon-acceptor (shiftf server nil)))
      (1am:is (check-address-port-usable))
      (1am:is (join-thread client-thread)))))

(1am:test abandon-at-after-handle-incoming-connection
  (with-making-test-server (server)
    (setf (after-handle-incoming-connection-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (1am:is (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-after-handle-incoming-connection)
      (schedule-waking-up-a-worker-in-after-handle-incoming-connection)
      (1am:is (abandon-acceptor (shiftf server nil)))
      (1am:is (check-address-port-usable))
      (1am:is (join-thread client-thread)))))


;;; With a broken threads

(1am:test abandon-with-a-finished-thread
  (with-making-test-server (server)
    (let* ((taskmaster (hunchentoot::acceptor-taskmaster server))
           (finished-thread
             (hunchentoot:start-thread taskmaster
                                       (constantly t)
                                       :name "Simulating a finished thread.")))
      (hunchentoot-recycling-taskmaster::add-recycling-taskmaster-thread
       taskmaster finished-thread)
      (1am:is (abandon-acceptor (shiftf server nil)))
      (1am:is (join-thread finished-thread)))))

(1am:test abandon-with-a-broken-thread
  (with-making-test-server (server)
    (setf (simulate-broken-thread-stop-p) t)
    (let* ((taskmaster (hunchentoot::acceptor-taskmaster server))
           (broken-thread
             (hunchentoot:start-thread taskmaster
                                       (lambda () (stop-at-simulate-broken-thread) t)
                                       :name "Simulating a broken thread.")))
      (hunchentoot-recycling-taskmaster::add-recycling-taskmaster-thread
       taskmaster broken-thread)
      (sleep-for-a-worker-in-simulate-broken-thread)
      (schedule-waking-up-a-worker-in-simulate-broken-thread 1)
      (1am:is (abandon-acceptor (shiftf server nil)))
      (1am:is (join-thread broken-thread)))))
