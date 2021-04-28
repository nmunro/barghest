(defsystem "barghest"
  :version "0.1.0"
  :author "NMunro"
  :license ""
  :depends-on (:woo
               :clack
               :djula)
  :components ((:module "src"
                :components
                ((:file "status")
                 (:file "http")
                 (:file "views")
                 (:file "crypt")
                 (:file "urls")
                 (:file "main"))))
  :description "")

(defsystem "barghest/tests"
  :author "NMunro"
  :license ""
  :depends-on ("barghest"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for barghest"
  :perform (test-op (op c) (symbol-call :rove :run c)))
