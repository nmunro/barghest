(defpackage barghest/models
  (:use :cl)
  (:export #:find-models-for-app))

(in-package barghest/models)

(defun find-models-for-app (app)
  (let ((pkg-name (string-upcase (format nil "~A/models" app))))
    (find-package pkg-name)))
