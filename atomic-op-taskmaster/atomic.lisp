(in-package #:hunchentoot-atomic-op-taskmaster)

(defclass atomic-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  ((hunchentoot::thread-count
    :type bt2:atomic-integer
    :initform (bt2:make-atomic-integer))
   (hunchentoot::accept-count
    :type bt2:atomic-integer
    :initform (bt2:make-atomic-integer)))
  (:documentation "A taskmaster works just like
`hunchentoot:one-thread-per-connection-taskmaster' except using
atomic integers."))

(in-package #:hunchentoot) ; because I use internal symbols of Hunchentoot.

(defmethod taskmaster-accept-count ((taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (bt2:atomic-integer-value (slot-value taskmaster 'accept-count)))

(defmethod (setf taskmaster-accept-count) (value (taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (setf (bt2:atomic-integer-value (slot-value taskmaster 'accept-count)) value))

(defmethod increment-taskmaster-accept-count ((taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (when (taskmaster-max-accept-count taskmaster)
    (bt2:atomic-integer-incf (slot-value taskmaster 'accept-count))))

(defmethod decrement-taskmaster-accept-count ((taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (when (taskmaster-max-accept-count taskmaster)
    (bt2:atomic-integer-decf (slot-value taskmaster 'accept-count))))

(defmethod taskmaster-thread-count ((taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (bt2:atomic-integer-value (slot-value taskmaster 'thread-count)))

(defmethod (setf taskmaster-thread-count) (value (taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (setf (bt2:atomic-integer-value (slot-value taskmaster 'thread-count)) value))

(defmethod increment-taskmaster-thread-count ((taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (when (taskmaster-max-thread-count taskmaster)
    (bt2:atomic-integer-incf (slot-value taskmaster 'thread-count))))

(defmethod decrement-taskmaster-thread-count ((taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (when (taskmaster-max-thread-count taskmaster)
    (prog2
        (bt2:atomic-integer-decf (slot-value taskmaster 'thread-count))
        (decrement-taskmaster-accept-count taskmaster)
      (when (and (taskmaster-max-accept-count taskmaster)
                 (< (taskmaster-thread-count taskmaster)
                    (taskmaster-max-accept-count taskmaster)))
        (note-free-connection taskmaster)))))


(in-package #:hunchentoot-atomic-op-taskmaster)

(defclass atomic-acceptor (hunchentoot:acceptor)
  ((hunchentoot::requests-in-progress
    :type bt2:atomic-integer
    :initform (bt2:make-atomic-integer)))
  (:documentation "An acceptor works just like
`hunchentoot:acceptor' except using atomic integers."))

(defvar *original-do-with-acceptor-request-count-incremented*
  (fdefinition 'hunchentoot::do-with-acceptor-request-count-incremented))

(in-package #:hunchentoot)

(defmethod acceptor-requests-in-progress ((acceptor hunchentoot-atomic-op-taskmaster:atomic-acceptor))
  (bt2:atomic-integer-value (slot-value acceptor 'requests-in-progress)))

(defmethod (setf acceptor-requests-in-progress) (value (acceptor hunchentoot-atomic-op-taskmaster:atomic-acceptor))
  (setf (bt2:atomic-integer-value (slot-value acceptor 'requests-in-progress)) value))

(defgeneric do-with-acceptor-request-count-incremented-gf (acceptor function)
  (:method (*acceptor* function)
    "Default method is same as the original one"
    (funcall hunchentoot-atomic-op-taskmaster:*original-do-with-acceptor-request-count-incremented*
             *acceptor* function))
  (:method ((*acceptor* hunchentoot-atomic-op-taskmaster:atomic-acceptor) function)
    "Do same as the original one except utilizing atomic integers."
    (with-slots (requests-in-progress) *acceptor*
      (bt2:atomic-integer-incf requests-in-progress)
      (unwind-protect
           (funcall function)
        (bt2:atomic-integer-decf requests-in-progress)
        (with-lock-held ((acceptor-shutdown-lock *acceptor*))
          (when (acceptor-shutdown-p *acceptor*)
            (condition-variable-signal (acceptor-shutdown-queue *acceptor*))))))))

(defun do-with-acceptor-request-count-incremented (*acceptor* function)
  "Replace original one to a trampoline to our generic function"
  (do-with-acceptor-request-count-incremented-gf *acceptor* function))

(in-package #:hunchentoot-recycling-taskmaster)

(defmethod count-busy-thread ((taskmaster hunchentoot-atomic-op-taskmaster:atomic-taskmaster))
  (let ((acceptor (hunchentoot::taskmaster-acceptor taskmaster)))
    (typecase acceptor
      (hunchentoot-atomic-op-taskmaster:atomic-acceptor
       (hunchentoot::acceptor-requests-in-progress acceptor))
      (otherwise
       (call-next-method)))))


(in-package #:hunchentoot-atomic-op-taskmaster)

;;; Derived classes
(defclass atomic-ssl-acceptor (atomic-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass atomic-easy-acceptor (atomic-acceptor hunchentoot:easy-acceptor)
  ())

(defclass atomic-easy-ssl-acceptor (atomic-easy-acceptor atomic-ssl-acceptor)
  ())
