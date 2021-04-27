(defpackage barghest/crypt
  (:use :cl)
  (:export #:generate-secret-key))
(in-package :barghest/crypt)

(defun generate-secret-key ()
  (let ((nums (loop :for num :from 48 :to 122 :collect (code-char num))))
    (format nil "~{~A~}" (loop :for num :from 0 :to 31 :collect (nth (random (length nums)) nums)))))
