(defsystem "barghest"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:usocket)
  :components ((:module "src"
                :components
                ((:file "datetime")
                 (:file "http")
                 (:file "request")
                 (:file "response")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "barghest/tests"))))

(defsystem "barghest/tests"
  :author ""
  :license ""
  :depends-on ("barghest"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for barghest"
  :perform (test-op (op c) (symbol-call :rove :run c)))
