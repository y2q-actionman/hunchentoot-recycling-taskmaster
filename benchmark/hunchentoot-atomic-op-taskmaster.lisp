(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; hunchentoot-atomic-op-taskmaster

(defun bench-hunchentoot-atomic-taskmaster ()
  (bench-hunchentoot-using-class 'hunchentoot:easy-acceptor
                                 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster
                                 "hunchentoot_atomic-taskmaster_default.log"
                                 :hunchentoot-atomic-op-taskmaster))

(defun bench-hunchentoot-atomic-acceptor ()
  (bench-hunchentoot-using-class 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                                 'hunchentoot:one-thread-per-connection-taskmaster
                                 "hunchentoot_atomic-acceptor_default.log"
                                 :hunchentoot-atomic-op-taskmaster))

(defun bench-hunchentoot-atomic-all ()
  (bench-hunchentoot-using-class 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                                 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster
                                 "hunchentoot_atomic-all_default.log"
                                 :hunchentoot-atomic-op-taskmaster))

;;; hunchentoot-recycling-taskmaster + atomic-op

(defun bench-hunchentoot-recycling-taskmaster-atomic-all
    (&optional (threads-list (list *hunchentoot-recycling-taskmaster-default-thread-count*)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
   'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
   "hunchentoot-recycling-taskmaster-atomic-all"
   :hunchentoot-atomic-op-taskmaster
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))

(defun bench-hunchentoot-recycling-taskmaster-atomic-acceptor
    (&optional (threads-list (list *hunchentoot-recycling-taskmaster-default-thread-count*)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
   'hunchentoot-recycling-taskmaster:recycling-taskmaster
   "hunchentoot-recycling-taskmaster-atomic-acceptor"
   :hunchentoot-atomic-op-taskmaster
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))

(defun bench-hunchentoot-recycling-taskmaster-atomic-taskmaster
    (&optional (threads-list (list *hunchentoot-recycling-taskmaster-default-thread-count*)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
   'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
   "hunchentoot-recycling-taskmaster-atomic-taskmaster"
   :hunchentoot-atomic-op-taskmaster
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))
