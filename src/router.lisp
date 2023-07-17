(defpackage barghest/routes
  (:use :cl)
  (:export #:+urls+
           #:defroute
           #:mount
           #:path))

(in-package barghest/routes)

(defparameter +urls+ '())

(defun defroute (app route fn &key (method :GET))
  (setf (ningle:route app route :method method) fn))

(defun path (url callback &key (method :GET) (name ""))
  (list :url url :view callback :method method :name name))

(defun mount (app urls &key (prefix ""))
  (dolist (url urls)
    (let ((route (defroute app (format nil "~A~A" prefix (getf url :url)) (getf url :view) :method (getf url :method))))
      (when (string/= (getf url :name) "")
        (push (getf url :url) +urls+)
        (push (getf url :name) +urls+)))))
