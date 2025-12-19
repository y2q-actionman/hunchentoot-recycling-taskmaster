(defsystem #:hunchentoot-recycling-taskmaster-benchmark
  :description "Benchmarks for hunchentoot-recycling-taskmaster and othres."
  :license "BSD 2-Clause"               ; follows hunchentoot.
  :author "Yokota Yuki"
  :depends-on (#:hunchentoot
               #:hunchentoot-recycling-taskmaster
               #:hunchentoot-atomic-op-taskmaster
               #:cl-tbnl-gserver-tmgr
               #:quux-hunchentoot
               #:house
               #:conserv
               ;; #:teepeedee2
               #:wookie
               #:woo)
  :pathname #.(make-pathname :directory '(:relative "benchmark"))
  :serial t
  :components ((:file "benchmark")))
