(in-package :cl-user)

(defpackage #:hunchentoot-recycling-taskmaster
  (:use #:cl #:alexandria)
  (:export
   #:recycling-taskmaster
   #:parallel-acceptor
   #:parallel-easy-acceptor
   #:parallel-ssl-acceptor
   #:parallel-easy-ssl-acceptor
   #:kill
   ;; exported for hunchentoot-atomic-op-taskmaster
   #:recycling-taskmaster-busy-thread-count
   #:recycling-taskmaster-busy-thread-count-cell
   #:recycling-taskmaster-busy-thread-count-lock
   #:recycling-taskmaster-busy-thread-count-queue
   #:recycling-taskmaster-shutdown-queue
   #:recycling-taskmaster-acceptor-process-lock))
