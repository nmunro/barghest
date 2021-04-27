(defpackage barghest/views
  (:use :cl)
  (:export #:client-error-404
           #:server-error-500))
(in-package :barghest/views)

(defun client-error-404 (request)
  '(404 (:content-type "text/plain") ("Not Found")))

(defun server-error-500 (request)
  '(500 (:content-type "text/plain") ("Internal Server Error")))
