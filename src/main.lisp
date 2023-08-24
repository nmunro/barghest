(defpackage main
  (:use :cl))

(in-package main)

(defun setup ()
  (settings:load-settings "TMP"))

(setup)
