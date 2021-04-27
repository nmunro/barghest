(defpackage barghest/http
  (:use :cl)
  (:export #:response
           #:make-response
           #:render))
(in-package :barghest/http)

(defclass response ()
  ((status  :initarg :status  :initform (error "Must have a status") :reader status)
   (headers :initarg :headers :initform (error "Must have headers")  :reader headers)
   (content :initarg :content :initform (error "Must have content")  :reader content)))

(defmethod print-object ((object response) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~A" (status object) (content object))))

(defun make-response (content &key (status-code 200) (headers '(:content-type "text/plain")))
  (make-instance 'response :content content :status (barghest/status:make-status status-code) :headers headers))

(defgeneric render (obj)
  (:documentation "Renders a response"))

(defmethod render ((obj response))
  `(,(barghest/status:code (status obj)) ,(headers obj) ,(list (content obj))))
