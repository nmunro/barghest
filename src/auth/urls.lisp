(defpackage barghest/auth/urls
  (:use :cl)
  (:export #:patterns))

(in-package barghest/auth/urls)

(defparameter patterns (list
  (barghest/routes:path "login/" #'barghest/auth/views:render-login :method :GET :name :login-get)
  (barghest/routes:path "login/" #'barghest/auth/views:process-login :method :POST :name :login-post)
  (barghest/routes:path "logout/" #'barghest/auth/views:logout :method :GET :name :logout)))
