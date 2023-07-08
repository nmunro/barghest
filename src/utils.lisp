(defpackage barghest/utils
  (:use :cl)
  (:export #:alist-to-plist))

(in-package barghest/utils)

(defun alist-to-plist (alist)
  (loop :for item :in alist
        :collect (intern (string-upcase (car item)) "KEYWORD")
        :collect (cdr item)))
