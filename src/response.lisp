(defpackage barghest.response
  (:use :cl)
  (:import-from :barghest.http :make-status-code)
  (:export #:response
           #:status
           #:header
           #:body
           #:render
           #:make-response))
(in-package :barghest.response)

(defclass response ()
  ((status      :initarg :status      :initform (make-status-code :200) :accessor status)
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
  (format nil "HTTP/1.1 ~A ~A" (barghest.http:code status) (barghest.http:description status)))

;; TODO: Need to add a second stream that can be written to independent of the socket stream
;; this is what a programmer can write to during building of a response
;; the render method will then copy the programmer writable stream to the socket stream
(defun render (response content)
  (let ((content-length (length content)))
    (format (body response)
            "~A~%Content-Type: text/html~%Content-Length: ~A~%~%~A"
            (build-status-line (status response))
            content-length
            content)))

(defun render-error (err res)
  (let ((e (make-status-code (barghest.http:err-code err))))
    (format t "ERROR: ~A -> ~A~%" (barghest.http:code e) (barghest.http:description e))
    (setf (status res) e)
    (render res (format nil "~A: ~A~%" (barghest.http:code e) (barghest.http:description e)))))

(defun make-response (stream)
  (make-instance 'response :body stream))
