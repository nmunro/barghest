(defpackage barghest
  (:use :cl)
  (:export #:start-project))
(in-package :barghest)

(defun start-project (name)
  (djula:add-template-directory (asdf:system-relative-pathname "barghest" "src/templates/"))

  (multiple-value-bind (root created)
      (ensure-directories-exist (build-path name :admin))

    (unless created
      (return-from start-project (format nil "A project called '~A' already exists!~%" name)))

    (let ((root-path (build-path name :root)))
      (write-file "asd"    "asd" root-path :name name :author (uiop:getenv "USER"))
      (write-file "README" "md"  root-path :name name :author (uiop:getenv "USER"))
      (rename-file (merge-pathnames (make-pathname :name "asd" :type "asd") root-path) (merge-pathnames (make-pathname :name name :type "asd") root-path)))

    (let ((src-path (build-path name :src)))
      (write-file "manage" "lisp" src-path :name name))

    (let ((admin-path (build-path name :admin)))
      (write-file "settings" "lisp" admin-path :name name)
      (write-file "routes"   "lisp" admin-path :name name)
      (write-file "views"    "lisp" admin-path :name name))

    (format nil "Starting project '~A' in ~A" name root)))

(defun build-path (name location)
  (labels ((build-root (name)
             (let ((src (format nil "~{~A~^/~}" (append (cdr (reverse (cdr (reverse (pathname-directory (asdf:system-source-directory :barghest)))))) `(,name)))))
               (make-pathname :directory `(:absolute ,src))))

           (build-src (name)
             (merge-pathnames (make-pathname :directory '(:relative "src")) (build-root name)))

           (build-admin (name)
             (merge-pathnames (make-pathname :directory `(:relative ,name)) (build-src name))))
    (cond
      ((eq location :root)
        (build-root name))

      ((eq location :src)
        (build-src name))

      ((eq location :admin)
       (build-admin name)))))

(defun write-file (name type location &rest vars &key &allow-other-keys)
  (let ((template (djula:compile-template* (format nil "~A.template" name)))
        (file (merge-pathnames (make-pathname :name name :type type) location)))
     (with-open-file (out file :direction :output :if-does-not-exist :create)
       (apply #'djula:render-template* (append `(,template ,out) vars)))))
