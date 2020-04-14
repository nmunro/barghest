(defpackage barghest.request
  (:use :cl)
  (:export #:request
           #:action
           #:path
           #:header
           #:args
           #:body
           #:make-request))
(in-package :barghest.request)

(defclass request ()
  ((action :initarg :action  :initform (error "No HTTP Method provided") :reader action)
   (path   :initarg :path    :initform (error "No Path provided")        :reader path)
   (header :initarg :header  :initform (make-hash-table :test #'equalp)  :reader header)
   (args   :initarg :args    :initform (make-hash-table :test #'equalp)  :reader args)
   (body   :initarg :body    :initform ""                                :reader body)))

(defmethod print-object ((object request) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: /~A" (action object) (path object))))

(defgeneric action (obj)
  (:documentation "Returns the request action"))

(defgeneric path (obj)
  (:documentation "Returns the request path"))

(defgeneric header (obj)
  (:documentation "Returns the request header"))

(defgeneric args (obj)
  (:documentation "Returns the request args"))

(defgeneric body (obj)
  (:documentation "Returns the request body"))

(defun make-request (stream)
  (let* ((req           (parse-status-line (read-line stream)))
         (action        (gethash "action" req))
         (parsed-path   (gethash "url" req))
         (parsed-header (get-header stream))
         (parsed-params (append (gethash "params" req) (get-content-params stream parsed-header)))
         (headers       (make-hash-table :test #'equalp))
         (args          (make-hash-table :test #'equalp)))

    (dolist (header parsed-header)
      (setf (gethash (string (first header)) headers) (rest header)))

    (dolist (param parsed-params)
      (setf (gethash (string (first param)) args) (rest param)))

    ;; Get the request body!
    ;; Remember to put a cond here to determine if its a form upload or a json push or something, detect the type of data
    ;; in the body
    (let ((body (make-sequence 'string (parse-integer (gethash "content-length" headers "0") :junk-allowed t))))
      (read-sequence body stream)
      (make-instance 'request :action action :path parsed-path :header headers :args args :body body))))

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

(defun parse-status-line (s)
  (let* ((url (subseq s (+ 2 (position #\Space s)) (position #\Space s :from-end t)))
         (args (position #\? url))
         (action (subseq s 0 (position #\Space s)))
         (data (make-hash-table :test #'equalp)))

      (setf (gethash "action" data) action)

      (if args
        (progn
          (setf (gethash "url" data) (subseq url 0 args))
          (setf (gethash "params" data) (parse-params (subseq url (1+ args)))))

        (progn
          (setf (gethash "url" data) url)
          (setf (gethash "params" data) '())))
    data))

(defun get-header (stream)
  (let* ((line (read-line stream))
         (header (let ((key-value-split (position #\: line)))
              (when key-value-split
                (cons (intern (string-upcase (subseq line 0 key-value-split)))
                      (subseq line (+ key-value-split 2)))))))
    (when header
      (cons header (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))
