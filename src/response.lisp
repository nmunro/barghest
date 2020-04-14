(defpackage barghest.response
  (:use :cl)
  (:export #:response
           #:status
           #:header
           #:body
           #:render
           #:make-response))
(in-package :barghest.response)

(defclass response ()
  ((status      :initarg :status      :initform (barghest.status-codes:make-status-code :200) :accessor status)
   (header      :initarg :header      :initform (make-hash-table)                             :accessor header)
   (body        :initarg :body        :initform ""                                            :accessor body)))

(defgeneric status (obj)
  (:documentation "Gets the status-code of the response object"))

(defgeneric header (obj)
  (:documentation "Gets the header of the response object"))

(defgeneric body (obj)
  (:documentation "Gets the body of the response object"))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (format stream "~A: /~A" (action response) (path response))))

(defun build-status-line (status)
  (format nil "HTTP/1.1 ~A ~A" (barghest.status-codes:code status) (barghest.status-codes:description status)))

(defun render (response content)
  (let ((content-length (length content)))
    (format (body response)
            "~A~%Content-Type: text/html~%Content-Length: ~A~%~%~A"
            (build-status-line (status response))
            content-length
            content)))

(defun make-response (stream)
  (make-instance 'response :body stream))
