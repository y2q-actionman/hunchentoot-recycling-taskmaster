(in-package #:hunchentoot-recycling-taskmaster-test)

;;; Define functions to stop worker threads at some point.

(defconstant +bitly-after-sleep+ 0.1)

(defmacro define-debug-stopper (name-prefix)
  "Defines functions to stop threads for testing `hunchentoot:shutdown'."
  (let ((boolean-storage-name (intern (format nil "*~A-~A*" name-prefix '#:stop-boolean)))
        (boolean-accessor-name (intern (format nil "~A-~A" name-prefix '#:stop-p)))
        (lock-name (intern (format nil "*~A-~A*" name-prefix '#:lock)))
        (condition-variable-name (intern (format nil "*~A-~A*" name-prefix '#:condition-variable)))
        (waiter-counter-name (intern (format nil "*~A-~A*" name-prefix '#:waiter-counter)))
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
       (defvar ,condition-variable-name
         (make-condition-variable :name ,(string condition-variable-name)))
       (defvar ,waiter-counter-name
         0)
       (defun ,boolean-accessor-name ()
         (with-lock-held (,lock-name)
           ,boolean-storage-name))
       (defun (setf ,boolean-accessor-name) (value)
         (with-lock-held (,lock-name)
           (setf ,boolean-storage-name value)
           (condition-notify ,condition-variable-name)))
       (defun ,stop-function-name ()
         (with-lock-held (,lock-name)
           (incf ,waiter-counter-name)
           (condition-notify ,condition-variable-name)
           (unwind-protect
                (loop
                  while ,boolean-storage-name
                  do (condition-wait ,condition-variable-name ,lock-name))
             (decf ,waiter-counter-name)
             (condition-notify ,condition-variable-name))))
       (defun ,sleep-for-a-worker-in-function-name ()
         (with-lock-held (,lock-name)
           (loop
             until (plusp ,waiter-counter-name)
             do (condition-wait ,condition-variable-name ,lock-name))))
       (defun ,schedule-waking-up-a-worker-function-name ()
         (make-thread (lambda ()
                        (sleep +bitly-after-sleep+)
                        (setf (,boolean-accessor-name) nil)))))))

(define-debug-stopper before-handle-incoming-connection)
(define-debug-stopper after-handle-incoming-connection)
(define-debug-stopper before-process-connection)
(define-debug-stopper after-process-connection)
(define-debug-stopper hello-world-handler)

;;; Server class definitions for debugging.

(defparameter *debug-server-initial-thread* 2)
(defparameter *debug-server-port* 62532)

(defclass recycling-taskmaster-debug (hunchentoot-recycling-taskmaster:recycling-taskmaster)
  ()
  (:default-initargs
   :initial-thread-count *debug-server-initial-thread*))

(defmethod hunchentoot:handle-incoming-connection :before ((taskmaster recycling-taskmaster-debug) client-connection)
  (stop-at-before-handle-incoming-connection))

(defmethod hunchentoot:handle-incoming-connection :after ((taskmaster recycling-taskmaster-debug) client-connection)
  (stop-at-after-handle-incoming-connection))

(defclass parallel-acceptor-debug (hunchentoot-recycling-taskmaster:parallel-acceptor)
  ()
  (:default-initargs
   :taskmaster (make-instance 'recycling-taskmaster-debug)))

(defmethod hunchentoot:process-connection :before ((acceptor parallel-acceptor-debug) socket)
  (stop-at-before-process-connection))

(defmethod hunchentoot:process-connection :after ((acceptor parallel-acceptor-debug) socket)
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

(defun test-request (&key keep-alive)
  (handler-case
      (drakma:http-request *test-server-url* :keep-alive keep-alive)
    (usocket:connection-reset-error ()
      (progn))))

;;; Test scenarios

(1am:test shutdown-with-no-accept
  (with-making-test-server (server)
    (progn)))

(1am:test shutdown-after-one-accept
  (with-making-test-server (server)
    (test-request)))

(1am:test shutdown-in-hello-world-handler
  (with-making-test-server (server)
    (setf (hello-world-handler-stop-p) t)
    (make-thread 'test-request)
    (sleep-for-a-worker-in-hello-world-handler)
    ;; Schedule waking up a worker thread bitly after `hunchentoot:stop'.
    (schedule-waking-up-a-worker-in-hello-world-handler)
    (hunchentoot:stop (shiftf server nil))))
