(in-package :cl-user)

(defpackage #:hunchentoot-recycling-taskmaster-benchmark
  (:use :cl)
  (:export #:prepare
           #:run
           #:+nproc+
           ;; Test parameters
           #:*wrk-duration*
           #:*wrk-threads-and-connections*
           #:*test-keep-alive*
           #:*test-no-keep-alive*
           #:*handler-sleep-seconds*))
