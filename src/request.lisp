(defpackage barghest.request
  (:use :cl)
  (:export #:request
           #:action
           #:path
           #:headers
           #:args
           #:body
           #:form
           #:files
           #:make-request))
(in-package :barghest.request)

(defclass request ()
  ((action  :initarg :action  :initform (error "No HTTP Method provided") :reader action)
   (path    :initarg :path    :initform (error "No Path provided")        :reader path)
   (headers :initarg :headers :initform (make-hash-table :test #'equalp)  :reader headers)
   (args    :initarg :args    :initform (make-hash-table :test #'equalp)  :reader args)
   (form    :initarg :form    :initform (make-hash-table :test #'equalp)  :reader form)
   (files   :initarg :files   :initform (make-hash-table :test #'equalp)  :reader files)
   (body    :initarg :body    :initform ""                                :reader body)))

(defmethod print-object ((object request) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: /~A" (action object) (path object))))

(defgeneric action (obj)
  (:documentation "Returns the request action"))

(defgeneric path (obj)
  (:documentation "Returns the request path"))

(defgeneric headers (obj)
  (:documentation "Returns the request headers"))

(defgeneric args (obj)
  (:documentation "Returns the request args"))

(defgeneric body (obj)
  (:documentation "Returns the request body"))

(defgeneric form (obj)
  (:documentation "Returns the post form data"))

(defgeneric files (obj)
  (:documentation "Returns the post files data"))

(defun multipart/form-data (action path headers args stream)
  "This is where files need to be dealt with"

  (make-instance 'request :action action :path path :headers headers :args args))
  ;(let ((body (make-array (parse-integer (gethash "content-length" headers "0") :junk-allowed t) :element-type '(unsigned-byte 8))))
  ;  (read-sequence body stream)
  ;  (make-instance 'request :action action :path path :headers headers :args args :body body)))

(defun application/x-www-form-urlencoded (action path headers args stream)
  "This doesn't deal with files, so just don't worry about those"

  (let ((body (make-sequence 'string (parse-integer (gethash "content-length" headers "0") :junk-allowed t))))
    (read-sequence body stream)
    (make-instance 'request :action action :path path :headers headers :args args :body body)))

(defun make-request (stream)
  (let* ((req           (parse-status-line (read-line stream)))
         (action        (gethash "action" req))
         (parsed-path   (gethash "url" req))
         (parsed-header (get-header stream))
         (parsed-params (append (gethash "params" req) (get-content-params stream parsed-header)))
         (headers       (make-hash-table :test #'equalp))
         (args          (make-hash-table :test #'equalp)))

    ; build and set the headers
    (dolist (header parsed-header)
      (setf (gethash (string (first header)) headers) (string-trim '(#\Return) (subseq (rest header) 0 (search ";" (rest header))))))

    ; build and set the get parameters
    (dolist (param parsed-params)
      (setf (gethash (string (first param)) args) (rest param)))

    (let ((content-type (gethash "content-type" headers "text/html")))
        (cond
          ((string= "multipart/form-data" content-type)
           (multipart/form-data action parsed-path headers args stream))

          (t (application/x-www-form-urlencoded action parsed-path headers args stream))))))

(defmethod initialize-instance :after ((request request) &rest args)
  "This function does some post-processing with the body parameter after the object has been created"
  ; Basically if there's data in the body and some how it represents form data, then extract the form data
  ; put it into form and files respectively, otherwise just leave it in the body for something else to consume

  (declare (ignore args)))

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
  (let* ((url    (subseq s (+ 2 (position #\Space s)) (position #\Space s :from-end t)))
         (args   (position #\? url))
         (action (subseq s 0 (position #\Space s)))
         (data   (make-hash-table :test #'equalp)))

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

(defparameter post-1 #p"~/quicklisp/local-projects/barghest/post-data-1.txt")
(defparameter post-2 #p"~/quicklisp/local-projects/barghest/post-data-2.txt")

(let ((lines (uiop:read-file-lines post-2)))
  (dolist (line lines)
    (format t "~A~%" line)))

(with-open-file (in post-1)
  (let ((req (make-request in)))
    (format t "~A~%" req)
    (maphash #'(lambda (k v) (format t "    ~A: ~A~%" k v)) (headers req))))
