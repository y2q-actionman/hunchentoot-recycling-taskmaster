(in-package #:hunchentoot-recycling-taskmaster-test)

;;; Define functions to stop worker threads at some point.

(defconstant +bitly-after-sleep+ 0.1)

(defmacro define-debug-stopper (name-prefix)
  "Defines functions to stop threads for testing `hunchentoot:shutdown'."
  (let ((boolean-storage-name (intern (format nil "*~A-~A*" name-prefix '#:stop-boolean)))
        (boolean-accessor-name (intern (format nil "~A-~A" name-prefix '#:stop-p)))
        (lock-name (intern (format nil "*~A-~A*" name-prefix '#:lock)))
        (boolean-cv-name (intern (format nil "*~A-~A*" name-prefix '#:boolean-condition-variable)))
        (waiter-counter-name (intern (format nil "*~A-~A*" name-prefix '#:waiter-counter)))
        (waiter-cv-name (intern (format nil "*~A-~A*" name-prefix '#:waiter-condition-variable)))
        (stop-function-name (intern (format nil "~A-~A" '#:stop-at name-prefix)))
        (sleep-for-a-worker-in-function-name
          (intern (format nil "~A-~A" '#:sleep-for-a-worker-in name-prefix)))
        (schedule-waking-up-a-worker-function-name
          (intern (format nil "~A-~A" '#:schedule-waking-up-a-worker-in name-prefix))))
    `(progn
       (defparameter ,boolean-storage-name
         nil)
       (defvar ,lock-name
         (make-lock ,(string lock-name)))
       (defvar ,boolean-cv-name
         (make-condition-variable :name ,(string boolean-cv-name)))
       (defvar ,waiter-counter-name
         0)
       (defvar ,waiter-cv-name
         (make-condition-variable :name ,(string waiter-cv-name)))
       (defun ,boolean-accessor-name ()
         (with-lock-held (,lock-name)
           ,boolean-storage-name))
       (defun (setf ,boolean-accessor-name) (value)
         (with-lock-held (,lock-name)
           (setf ,boolean-storage-name value)
           (condition-notify ,boolean-cv-name)))
       (defun ,stop-function-name ()
         (with-lock-held (,lock-name)
           (incf ,waiter-counter-name)
           (condition-notify ,waiter-cv-name)
           (unwind-protect
                (loop
                  while ,boolean-storage-name
                  do (condition-wait ,boolean-cv-name ,lock-name))
             (decf ,waiter-counter-name)
             (condition-notify ,waiter-cv-name))))
       (defun ,sleep-for-a-worker-in-function-name ()
         (with-lock-held (,lock-name)
           (loop
             until (plusp ,waiter-counter-name)
             do (condition-wait ,waiter-cv-name ,lock-name))))
       (defun ,schedule-waking-up-a-worker-function-name (&optional (seconds +bitly-after-sleep+))
         (make-thread (lambda ()
                        (sleep seconds)
                        (setf (,boolean-accessor-name) nil)))))))

(define-debug-stopper before-handle-incoming-connection)
(define-debug-stopper after-handle-incoming-connection)
(define-debug-stopper before-process-connection)
(define-debug-stopper after-process-connection)
(define-debug-stopper hello-world-handler)
(define-debug-stopper simulate-broken-thread)

;;; Server class definitions for debugging.

(defparameter *debug-server-initial-thread* 2)
(defparameter *debug-server-port* 62532)

(defclass recycling-taskmaster-debug (hunchentoot-recycling-taskmaster:recycling-taskmaster)
  ()
  (:default-initargs
   :initial-thread-count *debug-server-initial-thread*))

(defmethod hunchentoot:handle-incoming-connection :before ((taskmaster recycling-taskmaster-debug) client-connection)
  (declare (ignore client-connection))
  (stop-at-before-handle-incoming-connection))

(defmethod hunchentoot:handle-incoming-connection :after ((taskmaster recycling-taskmaster-debug) client-connection)
  (declare (ignore client-connection))
  (stop-at-after-handle-incoming-connection))

(defclass parallel-acceptor-debug (hunchentoot-recycling-taskmaster:parallel-acceptor)
  ()
  (:default-initargs
   :taskmaster (make-instance 'recycling-taskmaster-debug)))

(defmethod hunchentoot:process-connection :before ((acceptor parallel-acceptor-debug) socket)
  (declare (ignore socket))
  (stop-at-before-process-connection))

(defmethod hunchentoot:process-connection :after ((acceptor parallel-acceptor-debug) socket)
  (declare (ignore socket))
  (stop-at-after-process-connection))

(defclass parallel-easy-acceptor-debug (parallel-acceptor-debug hunchentoot:easy-acceptor)
  ())

(hunchentoot:define-easy-handler (hello-world :uri "/") ()
  (stop-at-hello-world-handler)
  (setf (hunchentoot:content-type*) "text/plain")
  "Hello, World!")

(defun make-test-server ()
  (make-instance 'parallel-easy-acceptor-debug
                 :message-log-destination nil
                 :access-log-destination nil
                 :port *debug-server-port*))

(defmacro with-making-test-server ((server) &body body)
  `(let ((,server (make-test-server)))
     (hunchentoot:start ,server)
     (unwind-protect
          (progn ,@body)
       (when ,server
         (hunchentoot:stop ,server)))))

(defparameter *test-server-url*
  (format nil "http://127.0.0.1:~D/" *debug-server-port*))

;;; Test scenarios

(1am:test shutdown-with-no-accept
  (with-making-test-server (server)
    (1am:is (hunchentoot:stop (shiftf server nil) :soft t))))

(1am:test shutdown-after-one-accept
  (with-making-test-server (server)
    (1am:is (drakma:http-request *test-server-url*))
    (1am:is (hunchentoot:stop (shiftf server nil) :soft t))))

;;; Simulates worker threads are working for a client connection somewhere.

(1am:test shutdown-at-before-handle-incoming-connection
  (with-making-test-server (server)
    (setf (before-handle-incoming-connection-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (1am:signals error
                             (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-before-handle-incoming-connection)
      (schedule-waking-up-a-worker-in-before-handle-incoming-connection)
      (1am:is (hunchentoot:stop (shiftf server nil) :soft t))
      (1am:is (join-thread client-thread)))))

(1am:test shutdown-at-before-process-connection
  (with-making-test-server (server)
    (setf (before-process-connection-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (1am:signals error
                             (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-before-process-connection)
      (schedule-waking-up-a-worker-in-before-process-connection)
      (1am:is (hunchentoot:stop (shiftf server nil) :soft t))
      (1am:is (join-thread client-thread)))))

(1am:test shutdown-in-hello-world-handler
  (with-making-test-server (server)
    (setf (hello-world-handler-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (1am:is (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-hello-world-handler)
      ;; Schedule waking up a worker thread bitly after `hunchentoot:stop'.
      (schedule-waking-up-a-worker-in-hello-world-handler)
      (1am:is (hunchentoot:stop (shiftf server nil) :soft t))
      (1am:is (join-thread client-thread)))))

(1am:test shutdown-at-after-process-connection
  (with-making-test-server (server)
    (setf (after-process-connection-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (1am:is (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-after-process-connection)
      (schedule-waking-up-a-worker-in-after-process-connection)
      (1am:is (hunchentoot:stop (shiftf server nil) :soft t))
      (1am:is (join-thread client-thread)))))

(1am:test shutdown-at-after-handle-incoming-connection
  (with-making-test-server (server)
    (setf (after-handle-incoming-connection-stop-p) t)
    (let ((client-thread
            (make-thread (lambda ()
                           (1am:is (drakma:http-request *test-server-url*))
                           t))))
      (sleep-for-a-worker-in-after-handle-incoming-connection)
      (schedule-waking-up-a-worker-in-after-handle-incoming-connection)
      (1am:is (hunchentoot:stop (shiftf server nil) :soft t))
      (1am:is (join-thread client-thread)))))

;;; With a broken threads

(1am:test shutdown-with-a-finished-thread
  (with-making-test-server (server)
    (let* ((taskmaster (hunchentoot::acceptor-taskmaster server))
           (finished-thread
             (hunchentoot:start-thread taskmaster
                                       (constantly t)
                                       :name "Simulating a finished thread.")))
      (hunchentoot-recycling-taskmaster::add-recycling-taskmaster-thread
       taskmaster finished-thread)
      (1am:is (hunchentoot:stop (shiftf server nil) :soft t))
      (1am:is (join-thread finished-thread)))))

(1am:test shutdown-with-a-broken-thread
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
      (1am:is (hunchentoot:stop (shiftf server nil) :soft t))
      (1am:is (join-thread broken-thread)))))
