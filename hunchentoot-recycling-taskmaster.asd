(defsystem #:hunchentoot-recycling-taskmaster
  :description "An experiment to improve multithreading performance of Hunchentoot without any additional dependencies."
  :license "BSD 2-Clause"               ; follows hunchentoot.
  :author "Yokota Yuki"
  :depends-on (#:hunchentoot)
  :pathname #.(make-pathname :directory '(:relative "recycling-taskmaster"))
  :serial t
  :components ((:file "package")
               (:file "taskmaster")
               (:file "acceptor")
               (:file "abandon-server"))
  :in-order-to ((test-op (test-op #:hunchentoot-recycling-taskmaster-test))))
