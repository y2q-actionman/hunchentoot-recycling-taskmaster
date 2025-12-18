(in-package :cl-user)

(defpackage #:hunchentoot-atomic-op-taskmaster
  (:use #:cl #:hunchentoot-recycling-taskmaster
        #:alexandria)
  (:export
   ;; On Hunchentoot
   #:atomic-taskmaster
   #:atomic-acceptor
   #:atomic-ssl-acceptor
   #:atomic-easy-acceptor
   #:atomic-easy-ssl-acceptor
   ;; On hunchentoot-recycling-taskmaster
   #:atomic-parallel-ssl-acceptor
   #:atomic-parallel-easy-acceptor
   #:atomic-parallel-easy-ssl-acceptor
   #:atomic-recycling-taskmaster
   #:atomic-parallel-acceptor
   ;; for internal use.
   #:*original-do-with-acceptor-request-count-incremented*))
