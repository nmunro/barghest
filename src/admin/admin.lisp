(defpackage barghest/admin/admin
  (:use :cl)
  (:export #:main))

(in-package barghest/admin/admin)

(defun register-model (app)
  (format t "Registering Model: ~A~%" app))

(defun main ()
  (register-model 'barghest/admin/models:user)
  (register-model 'barghest/admin/models:role))
