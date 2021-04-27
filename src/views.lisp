(defpackage barghest/views
  (:use :cl)
  (:export #:client-error-404
           #:server-error-500))
(in-package :barghest/views)

(defun client-error-404 (request)
  (let ((response (barghest/http:make-response "<h1>Sorry, not found</h1>" :status-code 404 :headers '(:content-type "text/html"))))
    (barghest/http:render response)))

(defun server-error-500 (request)
  (let ((response (barghest/http:make-response "<h1>Internal Server Error</h1>" :status-code 500 :headers '(:content-type "text/html"))))
    (barghest/http:render response)))
