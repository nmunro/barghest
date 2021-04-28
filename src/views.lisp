(defpackage barghest/views
  (:use :cl)
  (:export #:client-error-404
           #:server-error-500))
(in-package :barghest/views)

(defun client-error-404 (request)
  (let ((response (barghest/http:make-response (format nil "<h1>~A</h1>" (getf barghest/status:codes 404)) :status-code 404 :headers '(:content-type "text/html"))))
    (barghest/http:render response)))

(defun server-error-500 (request)
  (let ((response (barghest/http:make-response (format nil "<h1>~A</h1>" (getf barghest/status:codes 500)) :status-code 500 :headers '(:content-type "text/html"))))
    (barghest/http:render response)))
