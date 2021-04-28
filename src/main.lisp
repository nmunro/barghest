(defpackage barghest
  (:use :cl)
  (:export #:start-project))
(in-package :barghest)

(defun start-project (name)
  (djula:add-template-directory (asdf:system-relative-pathname "barghest" "src/templates/"))

  (multiple-value-bind (root created)
      (ensure-directories-exist (build-admin-path name))

    (unless created
      (return-from start-project (format nil "A project called '~A' already exists!~%" name)))

    (write-asd-file name)
    (write-readme-file name)
    (write-manage-file name)

    (let ((admin-path (build-admin-path name)))
      (write-settings-file admin-path name)
      (write-routes-file admin-path name)
      (write-views-file admin-path name))

    (format nil "Starting project '~A' in ~A" name root)))

(defun build-root-path (name)
  (let ((src (format nil "~{~A~^/~}" (append (cdr (reverse (cdr (reverse (pathname-directory (asdf:system-source-directory :barghest)))))) `(,name)))))
    (make-pathname :directory `(:absolute ,src))))

(defun build-src-path (name)
  (merge-pathnames (make-pathname :directory '(:relative "src")) (build-root-path name)))

(defun build-admin-path (name)
  (merge-pathnames (make-pathname :directory `(:relative ,name)) (build-src-path name)))

(defmacro write-file (name type location &optional vars)
  `(let ((template (djula:compile-template* (format nil "~A.template" ,name)))
         (file (merge-pathnames (make-pathname :name ,name :type ,type) ,location)))
     (with-open-file (out file :direction :output :if-does-not-exist :create)
       (apply #'djula:render-template* (append `(,template ,out) ,vars)))))

(defun write-asd-file (name)
  (write-file "asd" "asd" (build-root-path name) `(:name ,name :author ,(uiop:getenv "USER")))
  (rename-file (merge-pathnames (make-pathname :name "asd" :type "asd") (build-root-path name)) (merge-pathnames (make-pathname :name name :type "asd") (build-root-path name))))

(defun write-readme-file (name)
  (write-file "README" "md" (build-root-path name) `(:name ,name :author ,(uiop:getenv "USER"))))

(defun write-settings-file (location name)
  (write-file "settings" "lisp" location `(:name ,name)))

(defun write-views-file (location name)
  (write-file "views" "lisp" location `(:name ,name)))

(defun write-routes-file (location name)
  (write-file "routes" "lisp" location `(:name ,name)))

(defun write-manage-file (name)
  (write-file "manage" "lisp" (build-src-path name) `(:name ,name)))

;; debug code to quickly start a project
;(ql:quickload :barghest)
;(barghest:start-project "my-project")

;; debug code to quickly remove a project
;(let ((path (build-path "myproject")))
;  (uiop:delete-directory-tree (ensure-directories-exist path) :validate (ensure-directories-exist path)))
