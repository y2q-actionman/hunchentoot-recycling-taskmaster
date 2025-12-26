(in-package :cl-user)

(defpackage #:hunchentoot-recycling-taskmaster
  (:use #:cl #:alexandria)
  (:export
   #:*default-standby-thread-count*
   #:recycling-taskmaster
   #:parallel-acceptor
   #:parallel-easy-acceptor
   #:parallel-ssl-acceptor
   #:parallel-easy-ssl-acceptor
   ;; Only for handling a broken server.
   #:abandon-acceptor
   #:recycling-taskmaster-corrupted-error
   ))
