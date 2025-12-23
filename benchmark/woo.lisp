(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; Woo

(defun bench-woo (&optional (threads-list '(nil)))
  (loop
    for threads in threads-list
    as logname = (format nil "woo_threads-~D~@[-default~*~].log"
                         threads (eql threads nil))
    as thunk = (lambda ()
                 ;; from README
                 (woo:run
                  (lambda (env)
                    (declare (ignore env))
                    (handler-small-sleep)
                    '(200 (:content-type "text/plain") ("Hello, World")))
                  :worker-num threads))
    as server-thread = (bt:make-thread
                        thunk :name (format nil "Woo server thread ~A" threads))
    collect server-thread
    do (wait-for-starting-server)
       (unwind-protect
            (run-wrk "http://localhost:5000" logname :woo)
         (bt:destroy-thread server-thread))))

(defparameter *woo-callback-app*
  (lambda (_env)
    (declare (ignore _env))
    (lambda (callback)
      ;; This is works, but no meaning to sleep.
      (handler-small-sleep)
      (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))

(defun bench-woo-callback (&optional (threads-list '(nil)))
  (loop
    for threads in threads-list
    as logname = (format nil "woo-callback_threads-~D~@[-default~*~].log"
                         threads (eql threads nil))
    as thunk = (lambda ()
                 (woo:run
                  *woo-callback-app*
                  :worker-num threads))
    as server-thread = (bt:make-thread
                        thunk :name (format nil "Woo server thread ~A" threads))
    collect server-thread
    do (wait-for-starting-server)
       (unwind-protect
            (run-wrk "http://localhost:5000" logname :woo)
         (bt:destroy-thread server-thread))))


;; Making a thread like below don't works because `*EVLOOP*' is NIL.
;; (defparameter *woo-callback-threads-app*
;;   (lambda (_env)
;;     (declare (ignore _env))
;;     (lambda (callback)
;;       (bt:make-thread (lambda ()
;;                         (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))))

;; Binding like that does not works also.
;; (let ((evloop woo.ev:*evloop*))
;;   (bt:make-thread (lambda (&aux (woo.ev:*evloop* evloop))
;;                     (funcall callback '(200 (:content-type "text/plain") ("Hello, World"))))))

;; (defun bench-woo-callback-threads (&optional (threads-list '(nil)))
;;   (loop
;;     for threads in threads-list
;;     as logname = (format nil "woo-callback_threads-~D~@[-default~*~].log"
;;                          threads (eql threads nil))
;;     as server = (clack:clackup *woo-callback-threads-app* :server :woo)
;;     do (wait-for-starting-server)
;;        (unwind-protect
;;             (run-wrk "http://localhost:5000" logname)
;;          (clack:stop server))))
