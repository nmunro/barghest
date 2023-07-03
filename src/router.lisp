(defpackage barghest/routes
  (:use :cl)
  (:export #:defroute))

(in-package barghest/routes)

(defun defroute (app route fn &key (method :GET))
  (setf (ningle:route app route :method method) fn))
