(defpackage barghest.datetime
  (:use :cl)
  (:export #:datetime))
(in-package :barghest.datetime)

(defun datetime-now ()
  (multiple-value-bind
    (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (declare (ignore day-of-week))
    (declare (ignore dst-p))
    (format nil "~2,'0d/~2,'0d/~A ~2,'0d:~2,'0d:~2,'0d (UTC~@d)" date month year hour minute second (- tz))))
