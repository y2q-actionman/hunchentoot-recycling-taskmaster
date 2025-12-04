(in-package #:hunchentoot-atomic-op-taskmaster)

;;; only for benchmarking

(defclass nonatomic-recycling-taskmaster (recycling-taskmaster)
  ((hunchentoot-recycling-taskmaster::busy-thread-count-cell
    :initform 0))
  (:documentation "I found only 'busy-thread-count' slot
contributes the benchmark score. This class is only for
testing it."))

(defmethod recycling-taskmaster-busy-thread-count ((taskmaster nonatomic-recycling-taskmaster))
  (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
    (recycling-taskmaster-busy-thread-count-cell taskmaster)))

(defmethod increment-recycling-taskmaster-busy-thread-count ((taskmaster nonatomic-recycling-taskmaster))
  (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
    (incf (recycling-taskmaster-busy-thread-count-cell taskmaster))))

(defmethod decrement-recycling-taskmaster-busy-thread-count ((taskmaster nonatomic-recycling-taskmaster))
  (hunchentoot::with-lock-held ((recycling-taskmaster-busy-thread-count-lock taskmaster))
    (let ((rest (decf (recycling-taskmaster-busy-thread-count-cell taskmaster))))
      (when (<= rest 0)
        (hunchentoot::condition-variable-signal (recycling-taskmaster-busy-thread-count-queue taskmaster)))
      rest)))
