(defpackage barghest.http-header
  (:use :cl)
  (:export #:make-http-header))
(in-package :barghest.http-header)

(defclass http-header ()
  ((name    :initarg :name    :initform "" :reader name)
   (value   :initarg :value   :initform "" :reader value)
   (content :initarg :content :initform "" :reader content)))

(defmethod print-object ((object http-header) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~A" (name object) (value object))))

(defun parse-header (header)
  (let ((name (subseq header 0 (position #\: header)))
        (value (string-trim '(#\Space) (subseq header (1+ (position #\: header)) (length header)))))
    (values name value)))

(defun parse-value (value)
  (let* ((data (string-trim '(#\Space) (subseq value (1+ (position #\; value)) (length value)))))
    data))

(defun make-http-header (header)
  (multiple-value-bind (name value)
    (parse-header header)
    (make-instance 'http-header :name name :value value)))
