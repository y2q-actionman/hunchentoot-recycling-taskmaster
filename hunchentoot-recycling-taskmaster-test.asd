(defsystem #:hunchentoot-recycling-taskmaster-test
  :description "Tests for hunchentoot-recycling-taskmaster"
  :license "BSD 2-Clause"
  :author "Yokota Yuki"
  :version "0.0.1"
  :depends-on (#:hunchentoot-recycling-taskmaster
               #:1am
               #:hunchentoot/test)
  :pathname #.(make-pathname :directory '(:relative "test" "recycling-taskmaster-test"))
  :serial t
  :components ((:file "package")
               (:file "hunchentoot-tests")
               (:file "shutdown")
               (:file "abandon"))
  :perform (prepare-op :before (o c)
                       (set (find-symbol* '#:*tests* '#:1am) '()))
  :perform (test-op (o s)
                    (symbol-call '#:1am '#:run)))
