(defsystem "barghest"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:ningle
               :alexandria
               :mito
               :djula
               :cerberus)
  :components ((:module "src"
                :components
                ((:file "http")
                 (:file "crypt")
                 (:file "router")
                 (:file "controllers"))))
  :description "Generate a skeleton for modern project"
  :in-order-to ((test-op (test-op "barghest/tests"))))

(defsystem "barghest/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("barghest"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for barghest"
  :perform (test-op (op c) (symbol-call :rove :run c)))
