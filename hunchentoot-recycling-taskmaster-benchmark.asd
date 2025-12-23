(defsystem #:hunchentoot-recycling-taskmaster-benchmark
  :description "Benchmarks for hunchentoot-recycling-taskmaster and other Lisp web servers."
  :license "BSD 2-Clause"               ; follows hunchentoot.
  :author "Yokota Yuki"
  :version "0.0.1"
  :depends-on
  (;; Hunchentoot family
   #:hunchentoot
   #:hunchentoot-recycling-taskmaster
   #:hunchentoot-atomic-op-taskmaster
   #:cl-tbnl-gserver-tmgr
   #:quux-hunchentoot

   #:house
   ;; #:teepeedee2 ; causes crash at loading on SBCL and AllegroCL.

   ;; They have dependencies to external C libraries, and AllegroCL cannot load then.
   (:feature :sbcl #:conserv)
   (:feature :sbcl #:wookie)
   (:feature :sbcl #:woo))
  :pathname #.(make-pathname :directory '(:relative "benchmark"))
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "hunchentoot")
               (:file "hunchentoot-recycling-taskmaster")
               (:file "hunchentoot-atomic-op-taskmaster")
               (:file "cl-tbnl-gserver-tmgr")
               (:file "quux-hunchentoot")
               (:file "house")
               ;; TODO: teepeedee2
               (:file "conserv" :if-feature :sbcl)
               (:file "wookie" :if-feature :sbcl)
               (:file "woo" :if-feature :sbcl)
               (:file "run"))
  :perform (test-op (o s)
                    (symbol-call '#:hunchentoot-recycling-taskmaster-benchmark '#:prepare)
                    (symbol-call '#:hunchentoot-recycling-taskmaster-benchmark '#:run)))
