(in-package :cl-user)

(defpackage #:hunchentoot-recycling-taskmaster
  (:use #:cl #:alexandria)
  (:export
   #:*default-initial-thread-count*
   #:recycling-taskmaster
   #:parallel-acceptor
   #:parallel-easy-acceptor
   #:parallel-ssl-acceptor
   #:parallel-easy-ssl-acceptor
   ;; Only for handling a broken server.
   #:abandon-acceptor
   #:recycling-taskmaster-corrupted-error
   ;; exported for hunchentoot-atomic-op-taskmaster
   #:recycling-taskmaster-busy-thread-count
   #:recycling-taskmaster-busy-thread-count-cell
   #:recycling-taskmaster-busy-thread-count-lock
   #:recycling-taskmaster-busy-thread-count-queue
   #:recycling-taskmaster-acceptor-process-lock))
