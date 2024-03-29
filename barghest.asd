(defsystem "barghest"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:ningle
               :alexandria
               :mito
               :djula
               :clop
               :cl-fad
               :ppath
               :envy
               :str
               :cerberus)
  :components ((:module "src"
                :components
                ((:module "utils"
                  :components
                  ((:file "io")
                   (:file "tron")))
                 (:file "router")
                 (:file "models")
                 (:module "static"
                  :components
                  ((:file "main")))
                 (:file "settings")
                 (:file "http")
                 (:file "crypt")
                 (:file "controllers")
                 (:module "admin"
                  :components
                  ((:file "models")
                   (:file "controllers")
                   (:file "views")
                   (:file "urls")
                   (:file "auth")
                   (:file "admin")))
                 (:file "auth"))))
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
