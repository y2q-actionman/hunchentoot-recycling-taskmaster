(in-package :hunchentoot-recycling-taskmaster-benchmark)

;;; House

(house:define-handler (hello-world :content-type "text/plain") ()
  (handler-small-sleep)
  "Hello world!")

(defun bench-house ()
  (house::clean-sessions!)              ; Undocumented, but required..
  ;; (house::dispose-session-token-gen)    ; I wrote this func, but not works...
  (let ((server-thread
          (bt:make-thread
           (lambda () (house:start 4040))
           :name "House server thread")))
    (wait-for-starting-server)
    (unwind-protect
         (run-wrk "http://localhost:4040/hello-world" "house_default.log" :house)
      (bt:destroy-thread server-thread))
    server-thread))
