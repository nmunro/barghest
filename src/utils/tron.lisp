(defpackage barghest/utils/tron
  (:use :cl)
  (:export #:alist-to-plist
           #:split-into-pairs
           #:make-keyword
           #:get-project-path))

(in-package barghest/utils/tron)

(defun make-keyword (s)
  (intern (string-upcase s) "KEYWORD"))

(defun split-into-pairs (kws)
  (loop :for (k v) :on kws :by #'cddr :collect `(,k ,v)))

(defun alist-to-plist (alist)
  (loop :for item :in alist
        :collect (make-keyword (car item))
        :collect (cdr item)))

(defun get-project-path (project &key (path ""))
  (asdf:system-relative-pathname project path))
