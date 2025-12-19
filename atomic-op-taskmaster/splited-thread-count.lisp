(in-package #:hunchentoot-atomic-op-taskmaster)

;;; Broken?
(defclass splited-thread-count-recycling-taskmaster (recycling-taskmaster)
  ((hunchentoot-recycling-taskmaster::thread-count
    :initform (bt2:make-atomic-integer)
    :reader recycling-taskmaster-thread-count
    :documentation "The number of how many threads running. This is same with the number of keys in acceptor-process slot, but splited to avoid locking.")))

(defmethod add-recycling-taskmaster-thread ((taskmaster splited-thread-count-recycling-taskmaster) thread)
  (prog1 (call-next-method)
    (bt2:atomic-integer-incf (recycling-taskmaster-thread-count taskmaster))))

(defmethod remove-recycling-taskmaster-thread ((taskmaster splited-thread-count-recycling-taskmaster) thread)
  (when-let ((deleted? (call-next-method)))
    (bt2:atomic-integer-decf (recycling-taskmaster-thread-count taskmaster))
    deleted?))

(defmethod count-recycling-taskmaster-thread ((taskmaster splited-thread-count-recycling-taskmaster) &key lock)
  (declare (ignore lock))
  (bt2:atomic-integer-value (recycling-taskmaster-thread-count taskmaster)))

(defmethod delete-recycling-taskmaster-finished-thread ((taskmaster splited-thread-count-recycling-taskmaster))
  (when-let ((deleted-cnt (call-next-method)))
    (bt2:atomic-integer-decf (recycling-taskmaster-thread-count taskmaster)
                             deleted-cnt)
    deleted-cnt))

;;; Derived classes
(defclass atomic-splited-thread-count-recycling-taskmaster
    (splited-thread-count-recycling-taskmaster atomic-taskmaster)
  ())
