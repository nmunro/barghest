(defpackage barghest/app
  (:use :cl)
  (:export #:register-model
           #:register-static))

(in-package barghest/app)

(defun register-model (app)
  (format t "Registering Model: ~A~%" app))

(defun register-static (app)
  (format t "Registering Model: ~A~%" app))
