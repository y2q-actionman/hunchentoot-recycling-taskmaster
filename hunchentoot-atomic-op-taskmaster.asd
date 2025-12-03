(defsystem #:hunchentoot-atomic-op-taskmaster
  :description "An experiment to improve multithreading performance of Hunchentoot without any additional dependencies. -- supplimental atomic-op experiment."
  :license "BSD 2-Clause"               ; follows hunchentoot.
  :author "Yokota Yuki"
  :depends-on (#:hunchentoot-recycling-taskmaster)
  :pathname #.(make-pathname :directory '(:relative "atomic-op-taskmaster"))
  :serial t
  :components ((:file "package")
               (:file "atomic")
               (:file "nonatomic-recycling")
               (:file "atomic-recycling"))
  :in-order-to ((test-op (test-op #:hunchentoot-atomic-op-taskmaster-test))))
