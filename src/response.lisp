(defpackage barghest.response
  (:use :cl)
  (:export #:response
           #:status-code
           #:header
           #:content
           #:send-response
           #:make-response))
(in-package :barghest.response)

(defclass response ()
  ((status-code :initarg :status-code :initform "200 OK"          :accessor status-code)
   (header      :initarg :header      :initform (make-hash-table) :accessor header)
   (content     :initarg :content     :initform ""                :accessor content)))

(defgeneric status-code (obj)
  (:documentation "Gets the status-code of the response object"))

(defgeneric header (obj)
  (:documentation "Gets the header of the response object"))

(defgeneric content (obj)
  (:documentation "Gets the content of the response object"))

(defgeneric send-response (response data)
  (:documentation "This takes some data and writes it to the stream"))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (format stream "~A: /~A" (action response) (path response))))

(defmethod send-response ((response response) data)
  (format (content response) (format-data (status-code response) data)))

(defun make-response (stream)
  (make-instance 'response :content stream))

(defun format-data (status content)
  (let ((content-length (length content)))
    (format nil "HTTP/1.1 ~A~%Content-Type: text/html~%Content-Length: ~A~%~%~A" status content-length content)))
