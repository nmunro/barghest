(defpackage barghest/utils/io
  (:use :cl)
  (:export #:list-files
           #:guess-mime-type))

(in-package barghest/utils/io)

(defun list-files (path)
  (let ((files))
    (unless (probe-file path)
      (return-from list-files files))
    (cl-fad:walk-directory path (lambda (name) (push name files)) :directories t)
    files))

(defun guess-mime-type (path)
  (typecase path
    (string (mimes:mime (pathname path)))
    (pathname (mimes:mime path))
    (t (error "Unsupported type"))))
