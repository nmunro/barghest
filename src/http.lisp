(defpackage barghest/http
  (:use :cl)
  (:export #:http-response
           #:make-http-response))
(in-package :barghest/http)

(defclass http-response ()
  ((status  :initarg :status  :initform (error "Must have a status") :reader status)
   (headers :initarg :headers :initform (error "Must have headers")       :reader headers)
   (content :initarg :content :initform (error "Must have content")       :reader content)))

(defmethod print-object ((object http-response) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~A" (status object) (content object))))

(defun make-http-response (content &key (status-code 200) (headers nil))
  (make-instance 'http-response :content content :status (barghest/status:make-status status-code) :headers headers))

(defgeneric render (obj)
  (:documentation "Renders a response"))
