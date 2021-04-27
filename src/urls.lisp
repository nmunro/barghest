(defpackage barghest/urls
  (:use :cl)
  (:export #:make-path
           #:dispatcher))
(in-package :barghest/urls)

(defclass url ()
  ((path :initarg :path :initform "/" :reader path)))

(defmethod print-object ((object url) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (path object))))

(defclass path ()
  ((url      :initarg :url      :initform (error "Must provide a url")      :reader url)
   (callback :initarg :callback :initform (error "Must provide a callback") :reader callback)
   (name     :initarg :name     :initform (error "Must provide a name")     :reader name)))

(defmethod print-object ((object path) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A [name: ~A]" (url object) (name object))))

(defun make-url (url)
  (make-instance 'url :path url))

(defun make-path (url callback &key (name nil))
  (unless name
    (error "URL must be given a name"))

  (make-instance 'path :url url :callback callback :name name))

(defun dispatcher (urls request)
  (let ((view (find (getf request :path-info) urls :test #'(lambda (needle item) (string= needle (url item))))))
    (if (null view)
      (barghest/views:client-error-404 request)
      (handler-case (apply (callback view) `(,request))
        (error (condition)
          (progn
            (format t "Test: ~A~%" condition)
            (barghest/views:server-error-500 request)))))))
