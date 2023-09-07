(defpackage barghest/tags
  (:use :cl)
  (:export #:static))

(in-package barghest/tags)

(djula:def-tag-compiler :static (path)
  (let ((static-root (getf (envy:config :malaga/settings) :static-url)))
    (lambda (stream) (format stream "~A/~A" static-root path))))
