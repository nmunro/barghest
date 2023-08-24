(defpackage barghest/settings
  (:use :cl)
  (:export :load-settings))

(in-package barghest/settings)

(defun load-settings (project &key verbose)
  (let ((project (barghest/utils/tron:make-keyword project)))
    (load (asdf:system-relative-pathname project "src/settings.lisp") :verbose verbose)))
