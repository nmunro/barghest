(defpackage barghest/models
  (:use :cl)
  (:export #:find-package-for-app))

(in-package barghest/models)

(defun find-package-for-app (project app)
  (let ((pkg-name (string-upcase (format nil "~A/~A/models" project app))))
    (find-package pkg-name)))
