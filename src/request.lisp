(defpackage barghest.request
  (:use :cl)
  (:export #:request
           #:action
           #:path
           #:header
           #:params
           #:make-request))
(in-package :barghest.request)

(defclass request ()
  ((action :initarg :action  :initform "GET"             :reader action)
   (path   :initarg :path    :initform "/"               :reader path)
   (header :initarg :header  :initform (make-hash-table) :reader header)
   (params :initarg :params  :initform (make-hash-table) :reader params)))

(defmethod print-object ((object request) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: /~A" (action object) (path object))))

(defgeneric action (obj)
  (:documentation "Returns the url action"))

(defgeneric path (obj)
  (:documentation "Returns the url path"))

(defgeneric header (obj)
  (:documentation "Returns the url header"))

(defgeneric params (obj)
  (:documentation "Returns the url params"))

(defun make-request (stream)
  (let* ((parsed-url    (parse-url (read-line stream)))
         (parsed-path   (first parsed-url))
         (parsed-header (get-header stream))
         (parsed-params (append (rest parsed-url) (get-content-params stream parsed-header)))
         (headers (make-hash-table :test #'equalp))
         (params (make-hash-table :test #'equalp)))

    (dolist (header parsed-header)
      (setf (gethash (string (first header)) headers) (rest header)))

    (dolist (param parsed-params)
      (setf (gethash (string (first param)) params) (rest param)))

    (make-instance 'request :path parsed-path :header headers :params params)))

(defun http-char (c1 c2 &optional (default #\Space))
  "Return the code char of the parsed integer, or the default parameter"
  (let ((code (parse-integer (coerce (list c1 c2) 'string) :radix 16 :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  "Take a string and break it up and recursively break it down into http chars"
  (labels
    ((f (lst)
       (when lst
         (case (car lst)
           (#\% (cons (http-char (cadr lst) (caddr lst)) (f (cdddr lst))))
           (#\+ (cons #\Space (f (cdr lst))))
           (otherwise (cons (car lst) (f (cdr lst))))))))
      (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s (+ 2 (position #\Space s)) (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))
