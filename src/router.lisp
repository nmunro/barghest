(defpackage barghest/routes
  (:use :cl)
  (:export #:+urls+
           #:defroute
           #:mount
           #:get-url
           #:path))

(in-package barghest/routes)

(defparameter +urls+ (make-hash-table :test #'equal))

(defun defroute (app route fn &key (method :GET))
  (setf (ningle:route app route :method method) fn))

(defmethod path (url (callback list) &key (method :GET) (name ""))
  (loop :for item :in callback
        :collect (list :url (format nil "~A~A" url (or (getf item :url) ""))
                       :view (getf item :view)
                       :method (or (getf item :method) method)
                       :name (or (getf item :name) name))))

(defmethod path (url (callback function) &key (method :GET) (name ""))
  (list :url url :view callback :method method :name name))

(defun mount (app urls &key (prefix ""))
  (dolist (url urls)
    (if (member :url url)
        (mount-route app url :prefix prefix)
        (dolist (u url)
          (mount-route app u :prefix prefix)))))

(defun mount-route (app url &key (prefix ""))
  (let ((route (defroute app (format nil "~A~A" prefix (getf url :url)) (getf url :view) :method (getf url :method)))
        (id (getf url :name (getf url :url))))
    (setf (gethash id +urls+) url)))

(defun get-url (url)
  (gethash url +urls+))
