(defpackage barghest/tags
  (:use :cl))

(in-package barghest/tags)

(defun get-type (x)
  (etypecase x
    (string x)
    (number (write-to-string x))
    (symbol (djula::resolve-variable-phrase (list x)))))

(defun select-type (value)
  (etypecase value
    (symbol (case value
              (:t t)
              (:nil nil)
              (t (djula::resolve-variable-phrase (djula::parse-variable-phrase (string value)))))) ; @TODO: Bit type comes from here
    (string value)
    (bit (write-to-string value))
    (number (write-to-string value))))

(djula:def-tag-compiler :static (path)
  (lambda (stream)
    (multiple-value-bind (val val-error)
        (get-type path)
      (cond
        (val
         (format stream "~A/~A" (getf (envy:config :malaga/settings) :static-url) val))

        (val-error
         (format stream val-error))))))

(djula:def-tag-compiler :url (name &rest args)
  (flet ((process-args ()
           (loop :for arg :in args
                 :if (stringp (select-type arg))
                   :collect (select-type arg)
                 :else
                   :collect (write-to-string (select-type arg)))))
    (lambda (stream)
        (format stream (interpolate-url (getf (barghest/routes:get-url name) :url) (process-args))))))

(defun interpolate-url (url args)
  (cl-ppcre:do-matches-as-strings (match-var "\:[a-zA-Z0-9]*\/" url)
    (let ((arg (pop args)))
      (if (str:ends-with-p "/" arg)
        (setf url (str:replace-first match-var arg url))
        (setf url (str:replace-first match-var (format nil "~A/" arg) url)))))
  url)
