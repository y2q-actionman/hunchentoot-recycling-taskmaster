(in-package #:hunchentoot-atomic-op-taskmaster)

;;; Broken?
(defclass atomic-recycling-taskmaster (recycling-taskmaster atomic-taskmaster)
  ())

(defclass atomic-parallel-acceptor (parallel-acceptor atomic-acceptor)
  ()
  #+ ()
  (:default-initargs
   :taskmaster (make-instance 'atomic-recycling-taskmaster)))

;;; Derived classes
(defclass atomic-parallel-ssl-acceptor (atomic-parallel-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass atomic-parallel-easy-acceptor (atomic-parallel-acceptor hunchentoot:easy-acceptor)
  ())

(defclass atomic-parallel-easy-ssl-acceptor (atomic-parallel-easy-acceptor parallel-ssl-acceptor)
  ())

;;; Broken?
(defclass atomic-thread-count-recycling-taskmaster (recycling-taskmaster)
  ((thread-count
    :initform (bt2:make-atomic-integer)
    :reader recycling-taskmaster-thread-count
    :documentation "The number of how many threads running. This is same with the number of keys in acceptor-process slot, but splited to avoid locking.")
   (shutdown-queue-lock
    :initform (hunchentoot::make-lock "shutdown-queue-lock")
    :reader recycling-taskmaster-shutdown-queue-lock
    :documentation
    "A lock for protecting shutdown-queue.")))

(defmethod add-recycling-taskmaster-thread ((taskmaster atomic-thread-count-recycling-taskmaster) thread)
  (call-next-method)
  (bt2:atomic-integer-incf (recycling-taskmaster-thread-count taskmaster)))

(defmethod remove-recycling-taskmaster-thread ((taskmaster atomic-thread-count-recycling-taskmaster) thread)
  (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
    (let* ((table (hunchentoot::acceptor-process taskmaster)))
      (remhash thread table)))
  (let ((rest (bt2:atomic-integer-decf (recycling-taskmaster-thread-count taskmaster))))
    (when (<= rest 0)
      (hunchentoot::with-lock-held ((recycling-taskmaster-shutdown-queue-lock taskmaster))
        (when (<= (bt2:atomic-integer-value (recycling-taskmaster-thread-count taskmaster)) 0)
          (hunchentoot::condition-variable-signal (recycling-taskmaster-shutdown-queue taskmaster)))))
    rest))

(defmethod count-recycling-taskmaster-thread ((taskmaster atomic-thread-count-recycling-taskmaster) &key)
  (bt2:atomic-integer-value (recycling-taskmaster-thread-count taskmaster)))

(defmethod delete-recycling-taskmaster-finished-thread ((taskmaster atomic-thread-count-recycling-taskmaster))
  "Delete dead threads kept in TASKMASTER accidentally."
  (let ((deleted-cnt 0))
    (hunchentoot::with-lock-held ((recycling-taskmaster-acceptor-process-lock taskmaster))
      (loop
        with table = (hunchentoot::acceptor-process taskmaster)
        for thread being the hash-key of (hunchentoot::acceptor-process taskmaster)
        unless (bt:thread-alive-p thread)
          do (incf deleted-cnt)
          and do (remhash thread table)))
    (when (plusp deleted-cnt)
      (let ((rest (bt2:atomic-integer-decf (recycling-taskmaster-thread-count taskmaster)
                                           deleted-cnt)))
        (when (<= rest 0)
          (hunchentoot::with-lock-held ((recycling-taskmaster-shutdown-queue-lock taskmaster))
            (when (<= (count-recycling-taskmaster-thread taskmaster) 0)
              (hunchentoot::condition-variable-signal
               (recycling-taskmaster-shutdown-queue taskmaster)))))))
    (plusp deleted-cnt)))

(defmethod wait-for-recycling-taskmaster-shutdown ((taskmaster atomic-thread-count-recycling-taskmaster))
  (when (plusp (count-recycling-taskmaster-thread taskmaster))
    (hunchentoot::with-lock-held ((recycling-taskmaster-shutdown-queue-lock taskmaster))
      (when (plusp (count-recycling-taskmaster-thread taskmaster))
        (hunchentoot::condition-variable-wait
         (recycling-taskmaster-shutdown-queue taskmaster)
         (recycling-taskmaster-shutdown-queue-lock taskmaster))))))
