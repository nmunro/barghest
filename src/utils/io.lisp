(defpackage barghest/utils/io
  (:use :cl)
  (:export #:list-files
           #:guess-mime-type))

(in-package barghest/utils/io)

;; (defun is-directory (file)
;;   (not (pathname-name file)))

;; (defun is-file (file)
;;   (pathname-name file))

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
