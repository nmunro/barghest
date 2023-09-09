(defpackage barghest/tags
  (:use :cl))

(in-package barghest/tags)

(defun get-type (x)
  (etypecase x
    (string x)
    (number (write-to-string x))
    (symbol (djula::resolve-variable-phrase (list x)))))

;; This now works with variables as well as strings!
(djula:def-tag-compiler :static (path)
  (lambda (stream)
    (multiple-value-bind (val val-error)
        (get-type path)
      (cond
        (val
         (format stream "~A/~A" (getf (envy:config :malaga/settings) :static-url) val))

        (val-error
         (format stream "~A" val-error))))))

(djula:def-tag-compiler :url (name &rest args)
  (lambda (stream)
    (let ((args (loop :for arg :in args :collect (get-type arg))))
      (format stream (interpolate-url (getf (barghest/routes:get-url name) :url) args)))))

(defun interpolate-url (url args)
  (cl-ppcre:do-matches-as-strings (match-var "\:[a-zA-Z0-9]*\/" url)
    (let ((arg (pop args)))
      (if (str:ends-with-p "/" arg)
        (setf url (str:replace-first match-var arg url))
        (setf url (str:replace-first match-var (format nil "~A/" arg) url)))))
  url)
