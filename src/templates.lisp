(defpackage barghest.templates
  (:use :cl)
  (:export #:load-template))
(in-package :barghest.templates)

(defun load-template (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
