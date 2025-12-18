(in-package #:hunchentoot-recycling-taskmaster-test)

(defmacro define-debug-stopper (name-prefix)
  "Defines functions to stop threads for testing `hunchentoot:shutdown'."
  (let ((boolean-storage-name (intern (format nil "*~A-~A*" name-prefix '#:stop-boolean)))
        (boolean-accessor-name (intern (format nil "~A-~A" name-prefix '#:stop-p)))
        (lock-name (intern (format nil "*~A-~A*" name-prefix '#:lock)))
        (condition-variable-name (intern (format nil "*~A-~A*" name-prefix '#:condition-variable)))
        (wait-function-name (intern (format nil "~A-~A" name-prefix '#:wait)))
        (notify-function-name (intern (format nil "~A-~A" name-prefix '#:notify))))
    `(progn
       (defparameter ,boolean-storage-name
         nil)
       (defun ,boolean-accessor-name ()
         ,boolean-storage-name)
       (defun (setf ,boolean-accessor-name) (value)
         (setf ,boolean-storage-name value))
       (defvar ,lock-name
         (make-lock ,(string lock-name)))
       (defvar ,condition-variable-name
         (make-condition-variable :name ,(string condition-variable-name)))
       (defun ,wait-function-name ()
         (with-lock-held (,lock-name)
           (loop
             while (,boolean-accessor-name)
             do (condition-wait ,condition-variable-name ,lock-name))))
       (defun ,notify-function-name ()
         (with-lock-held (,lock-name)
           (condition-notify ,condition-variable-name))))))

(define-debug-stopper before-handle-incoming-connection)
(define-debug-stopper after-handle-incoming-connection)
(define-debug-stopper before-process-connection)
(define-debug-stopper after-process-connection)


(defparameter *debug-server-initial-thread* 2)
(defparameter *debug-server-port* 62532)

(defclass recycling-taskmaster-debug (hunchentoot-recycling-taskmaster:recycling-taskmaster)
  ()
  (:default-initargs
   :initial-thread-count *debug-server-initial-thread*))

(defmethod hunchentoot:handle-incoming-connection :before ((taskmaster recycling-taskmaster-debug) client-connection)
  (before-handle-incoming-connection-wait))

(defmethod hunchentoot:handle-incoming-connection :after ((taskmaster recycling-taskmaster-debug) client-connection)
  (after-handle-incoming-connection-wait))

(defclass parallel-acceptor-debug (hunchentoot-recycling-taskmaster:parallel-acceptor)
  ()
  (:default-initargs
   :taskmaster (make-instance 'recycling-taskmaster-debug)))

(defmethod hunchentoot:process-connection :before ((acceptor parallel-acceptor-debug) socket)
  (before-process-connection-wait))

(defmethod hunchentoot:process-connection :after ((acceptor parallel-acceptor-debug) socket)
  (after-process-connection-wait))

(defclass parallel-easy-acceptor-debug (parallel-acceptor-debug hunchentoot:easy-acceptor)
  ())

(hunchentoot:define-easy-handler (hello-world :uri "/") ()
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

(1am:test no-accept-shutdown
  (with-making-test-server (server)
    (progn)))
