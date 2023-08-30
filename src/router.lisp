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
  (let ((route (defroute app (format nil "~A~A" prefix (getf url :url)) (getf url :view) :method (getf url :method))))
      (when (string/= (getf url :name) "")
        (push (getf url :url) +urls+)
        (push (getf url :name) +urls+))))
