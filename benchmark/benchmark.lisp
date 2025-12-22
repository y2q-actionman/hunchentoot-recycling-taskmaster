(in-package :hunchentoot-recycling-taskmaster-benchmark)

(defconstant +sleep-seconds+ 0.0001)        ; 0 or 0.0001. TODO: add Wookie support.

(defun wait-for-starting-server ()
  (sleep 1))

(defun handler-small-sleep ()
  (when (plusp +sleep-seconds+)
    (sleep +sleep-seconds+)))

;;; Hunchentoot

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (handler-small-sleep)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defun bench-hunchentoot ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_default.log")
      (hunchentoot:stop server :soft t))
    server))

;;; hunchentoot-atomic-op-taskmaster

(defun bench-hunchentoot-atomic-taskmaster ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-taskmaster_default.log")
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot-atomic-acceptor ()
  (let ((server (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242)))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-acceptor_default.log")
      (hunchentoot:stop server :soft t))
    server))

(defun bench-hunchentoot-atomic-all ()
  (let ((server (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "hunchentoot_atomic-all_default.log")
      (hunchentoot:stop server :soft t))
    server))

;;; hunchentoot-recycling-taskmaster

(defparameter *hunchentoot-recycling-taskmaster-default-thread-count*
  hunchentoot-recycling-taskmaster::*default-initial-thread-count*)

(defun bench-hunchentoot-recycling-taskmaster (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycling-taskmaster:recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

;;; hunchentoot-recycling-taskmaster + atomic-op

(defun bench-hunchentoot-recycling-taskmaster-atomic-all (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster-atomic-all_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

(defun bench-hunchentoot-recycling-taskmaster-atomic-acceptor (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster-atomic-acceptor_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-recycling-taskmaster:recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

(defun bench-hunchentoot-recycling-taskmaster-atomic-taskmaster (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "hunchentoot-recycling-taskmaster-atomic-taskmaster_threads-~A~@[-default~*~].log"
                         threads
                         (eql threads *hunchentoot-recycling-taskmaster-default-thread-count*))
    as taskmaster = (make-instance 'hunchentoot-atomic-op-taskmaster:atomic-recycling-taskmaster
                                   :initial-thread-count threads)
    as server = (make-instance 'hunchentoot-recycling-taskmaster:parallel-easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster taskmaster)
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

;;; cl-tbnl-gserver-tmgr

(defparameter *cl-tbnl-gserver-tmgr-default-thread-count*
  cl-tbnl-gserver-tmgr.tmgr::*gserver-tmgr-poolsize*)

(defun bench-cl-tbnl-gserver-tmgr (&optional (threads-list '(8)))
  (loop
    for threads in threads-list
    as logname = (format nil "cl-tbnl-gserver-tmgr_threads-~D~@[-default~*~].log"
                         threads (eql threads *cl-tbnl-gserver-tmgr-default-thread-count*))
    as server = (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'cl-tbnl-gserver-tmgr.tmgr:gserver-tmgr
                                                          :max-thread-count threads))
    collect server
    do (hunchentoot:start server)
       (unwind-protect
            (run-wrk "http://localhost:4242/yo" logname)
         (hunchentoot:stop server :soft t))))

;;; quux-hunchentoot

(defun bench-quux-hunchentoot ()
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :message-log-destination nil
                               :access-log-destination nil
                               :port 4242
                               :taskmaster (make-instance 'quux-hunchentoot:thread-pooling-taskmaster))))
    (hunchentoot:start server)
    (unwind-protect
         (run-wrk "http://localhost:4242/yo" "quux-hunchentoot_default.log")
      (hunchentoot:stop server :soft t))
    server))

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
