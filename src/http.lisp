(defpackage barghest/http
  (:use :cl)
  (:export #:redirect
           #:forbidden
           #:render))

(in-package barghest/http)

(defun render (template &rest kws &key &allow-other-keys)
  (let ((template (djula:compile-template* template)))
    (apply #'djula:render-template* (append `(,template nil) kws))))

(defun redirect (url)
  (setf (lack.response:response-headers ningle:*response*) (append (lack.response:response-headers ningle:*response*) (list "Location" url)))
  (setf (lack.response:response-status ningle:*response*) "303"))

(defun forbidden (template &key msg)
  (setf (lack.response:response-status ningle:*response*) "403")
  (render template :msg msg))
