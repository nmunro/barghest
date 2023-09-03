(defpackage barghest/admin/urls
  (:use :cl)
  (:export #:patterns))

(in-package barghest/admin/urls)

(defparameter patterns (list
  (barghest/routes:path "login/" #'barghest/auth/views:login :method :POST :name :login)
  (barghest/routes:path "logout/" #'barghest/auth/views:logout :method :GET :name :logout)))
