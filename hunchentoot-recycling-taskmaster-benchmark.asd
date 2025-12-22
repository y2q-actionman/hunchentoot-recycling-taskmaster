(defsystem #:hunchentoot-recycling-taskmaster-benchmark
  :description "Benchmarks for hunchentoot-recycling-taskmaster and other Lisp web servers."
  :license "BSD 2-Clause"               ; follows hunchentoot.
  :author "Yokota Yuki"
  :depends-on
  (;; Hunchentoot family
   #:hunchentoot
   #:hunchentoot-recycling-taskmaster
   #:hunchentoot-atomic-op-taskmaster
   #:cl-tbnl-gserver-tmgr
   #:quux-hunchentoot

   #:house
   ;; #:teepeedee2 ; causes crash at loading on SBCL.

   ;; They have dependencies to external C libraties.
   #:conserv
   #:wookie
   #:woo)
  :pathname #.(make-pathname :directory '(:relative "benchmark"))
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "benchmark"))
  :perform (test-op (o s)
                    (symbol-call '#:hunchentoot-recycling-taskmaster-benchmark '#:bench-all)))
