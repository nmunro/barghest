(defpackage barghest/views
  (:use :cl)
  (:export #:client-error-404
           #:server-error-500))
(in-package :barghest/views)

(defun client-error-404 (request)
  '(404 (:content-type "text/plain" :x-debug-header "Yo") ("Not Found")))

(defun server-error-500 (request)
  '(500 (:content-type "text/plain" :x-debug-header "yo") ("Internal Server Error")))
