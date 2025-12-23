(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; Wookie

;; (defun bench-wookie-static ()
;;   (let ((server-thread
;;           (bt:make-thread
;;            #'wookie-helper:start-static-server ; I found it in wookie-helper package.
;;            :name "Wookie server thread")))
;;     (wait-for-starting-server)
;;     (unwind-protect
;;          (run-wrk "http://localhost:8080" "wookie_static.log")
;;       (bt:destroy-thread server-thread))
;;     server-thread))

(wookie:defroute (:get "/hello-world") (req res)
  (declare (ignore req))
  ;; see `handler-small-sleep'
  (flet ((thunk ()
           (wookie:send-response res :body "Hello, World!")))
    (if (plusp *handler-sleep-seconds*)
        (cl-async:delay #'thunk
          :time *handler-sleep-seconds*)
        (thunk))))

(defun start-wookie-server ()     ; from wookie-doc/views/doc/index.md
  (let (listener server)
    (unwind-protect
         (as:with-event-loop ()
           (setf listener (make-instance 'wookie:listener :bind nil :port 8080)
                 server (wookie:start-server listener)))
      (as:close-tcp-server server))))

;; FIXME: This definition seems not supporting "Connection: close".
(defun bench-wookie ()
  (let ((server-thread
          (bt:make-thread
           (lambda () (start-wookie-server))
           :name "Wookie server thread")))
    (wait-for-starting-server)
    (unwind-protect
         (progn
           ;; (uiop:run-program "curl -v http://localhost:8080/hello-world"
           ;;                   :output :interactive)
           (run-wrk "http://localhost:8080/hello-world" "wookie_default.log"))
      (bt:destroy-thread server-thread))
    server-thread))
