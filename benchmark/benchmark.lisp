(in-package :hunchentoot-recycling-taskmaster-benchmark)

(defconstant +sleep-seconds+ 0.0001)        ; 0 or 0.0001. TODO: add Wookie support.

(defun wait-for-starting-server ()
  (sleep 1))

(defun handler-small-sleep ()
  (when (plusp +sleep-seconds+)
    (sleep +sleep-seconds+)))

;;; Hunchentoot and its families.

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (handler-small-sleep)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defun bench-hunchentoot-using-class (acceptor-class taskmaster-class log-file-name
                                      &key taskmaster-args)
  (assert (subtypep acceptor-class 'hunchentoot:easy-acceptor))
  (let ((server (make-instance acceptor-class
                               :taskmaster (apply #'make-instance taskmaster-class taskmaster-args)
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" log-file-name)
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot ()
  (bench-hunchentoot-using-class 'hunchentoot:easy-acceptor
                                 'hunchentoot:one-thread-per-connection-taskmaster
                                 "hunchentoot_default.log"))

;;; hunchentoot-atomic-op-taskmaster

(defun bench-hunchentoot-atomic-taskmaster ()
  (bench-hunchentoot-using-class 'hunchentoot:easy-acceptor
                                 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster
                                 "hunchentoot_atomic-taskmaster_default.log"))

(defun bench-hunchentoot-atomic-acceptor ()
  (bench-hunchentoot-using-class 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                                 'hunchentoot:one-thread-per-connection-taskmaster
                                 "hunchentoot_atomic-acceptor_default.log"))

(defun bench-hunchentoot-atomic-all ()
  (bench-hunchentoot-using-class 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                                 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster
                                 "hunchentoot_atomic-all_default.log"))

;;; hunchentoot-recycling-taskmaster

(defparameter *hunchentoot-recycling-taskmaster-default-thread-count*
  hunchentoot-recycling-taskmaster::*default-initial-thread-count*)

(defun bench-hunchentoot-family-per-threads (acceptor-class taskmaster-class logname-prefix
                                             taskmaster-thread-argname threads-list
                                             threads-default-count)
  (loop
    for threads in threads-list
    as logname = (format nil "~A_threads-~A~@[-default~*~].log"
                         logname-prefix threads
                         (eql threads threads-default-count))
    collect
    (bench-hunchentoot-using-class acceptor-class taskmaster-class logname
                                   :taskmaster-args (list taskmaster-thread-argname threads))))

(defun bench-hunchentoot-recycling-taskmaster (&optional (threads-list '(8)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
   'hunchentoot-recycling-taskmaster:recycling-taskmaster
   "hunchentoot-recycling-taskmaster"
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))

;;; hunchentoot-recycling-taskmaster + atomic-op

(defun bench-hunchentoot-recycling-taskmaster-atomic-all (&optional (threads-list '(8)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
   'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
   "hunchentoot-recycling-taskmaster-atomic-all"
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))

(defun bench-hunchentoot-recycling-taskmaster-atomic-acceptor (&optional (threads-list '(8)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
   'hunchentoot-recycling-taskmaster:recycling-taskmaster
   "hunchentoot-recycling-taskmaster-atomic-acceptor"
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))

(defun bench-hunchentoot-recycling-taskmaster-atomic-taskmaster (&optional (threads-list '(8)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
   'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
   "hunchentoot-recycling-taskmaster-atomic-taskmaster"
   :initial-thread-count threads-list
   *hunchentoot-recycling-taskmaster-default-thread-count*))

;;; cl-tbnl-gserver-tmgr

(defparameter *cl-tbnl-gserver-tmgr-default-thread-count*
  cl-tbnl-gserver-tmgr.tmgr::*gserver-tmgr-poolsize*)

(defun bench-cl-tbnl-gserver-tmgr (&optional (threads-list '(8)))
  (bench-hunchentoot-family-per-threads
   'hunchentoot:easy-acceptor
   'cl-tbnl-gserver-tmgr.tmgr:gserver-tmgr
   "cl-tbnl-gserver-tmgr"
   :max-thread-count threads-list
   *cl-tbnl-gserver-tmgr-default-thread-count*))

;;; quux-hunchentoot

(defun bench-quux-hunchentoot ()
  (bench-hunchentoot-using-class 'hunchentoot:easy-acceptor
                                 'quux-hunchentoot:thread-pooling-taskmaster
                                 "quux-hunchentoot.log"))

;;; House

(house:define-handler (hello-world :content-type "text/plain") ()
  (handler-small-sleep)
  "Hello world!")

(defun bench-house ()
  (let ((server-thread
          (bt:make-thread
           (lambda () (house:start 4040))
           :name "House server thread")))
    (wait-for-starting-server)
    (unwind-protect
         (run-wrk "http://localhost:4040" "house_default.log")
      (bt:destroy-thread server-thread))
    server-thread))
