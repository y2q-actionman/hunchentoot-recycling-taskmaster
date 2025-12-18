(in-package #:hunchentoot-atomic-op-taskmaster)

;;; Broken?
(defclass atomic-recycling-taskmaster (recycling-taskmaster atomic-taskmaster)
  ())

(defclass atomic-parallel-acceptor (parallel-acceptor atomic-acceptor)
  ())

;;; Derived classes
(defclass atomic-parallel-ssl-acceptor (atomic-parallel-acceptor hunchentoot:ssl-acceptor)
  ())

(defclass atomic-parallel-easy-acceptor (atomic-parallel-acceptor hunchentoot:easy-acceptor)
  ())

(defclass atomic-parallel-easy-ssl-acceptor (atomic-parallel-easy-acceptor parallel-ssl-acceptor)
  ())

;;; Broken?
(defclass atomic-thread-count-recycling-taskmaster (recycling-taskmaster)
  ((hunchentoot-recycling-taskmaster::thread-count
    :initform (bt2:make-atomic-integer)
    :reader recycling-taskmaster-thread-count
    :documentation "The number of how many threads running. This is same with the number of keys in acceptor-process slot, but splited to avoid locking.")))

(defmethod add-recycling-taskmaster-thread ((taskmaster atomic-thread-count-recycling-taskmaster) thread)
  (prog1 (call-next-method)
    (bt2:atomic-integer-incf (recycling-taskmaster-thread-count taskmaster))))

(defmethod remove-recycling-taskmaster-thread ((taskmaster atomic-thread-count-recycling-taskmaster) thread)
  (when-let ((deleted? (call-next-method)))
    (bt2:atomic-integer-decf (recycling-taskmaster-thread-count taskmaster))
    deleted?))

(defmethod count-recycling-taskmaster-thread ((taskmaster atomic-thread-count-recycling-taskmaster) &key lock)
  (declare (ignore lock))
  (bt2:atomic-integer-value (recycling-taskmaster-thread-count taskmaster)))

(defmethod delete-recycling-taskmaster-finished-thread ((taskmaster atomic-thread-count-recycling-taskmaster))
  (when-let ((deleted-cnt (call-next-method)))
    (bt2:atomic-integer-decf (recycling-taskmaster-thread-count taskmaster)
                             deleted-cnt)
    deleted-cnt))
