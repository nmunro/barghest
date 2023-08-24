(defpackage barghest/static
  (:use :cl)
  (:export #:build-path-for-app
           #:build-static-path-for-app
           #:get-static-files
           #:generate-static-urls
           #:prepare-static-routes
           #:patterns))

(in-package barghest/static)

(defparameter patterns '())

(defun build-path-for-app (app resource-type)
  (pathname (ppath:join "src" app resource-type app)))

(defun build-static-path-for-app (project app)
  (asdf:system-relative-pathname project (build-path-for-app app "static")))

(defun get-static-files (project app)
  (barghest/utils/io:list-files (build-static-path-for-app project app)))

(defun generate-static-urls (project app)
  (let ((static-path (namestring (build-static-path-for-app project app)))
        (static-app-prefix (format nil "~A~A" (pathname app) (pathname-utils:directory-separator))))
    (loop :for static-file :in (get-static-files project app)
          :unless (ppath:isdir static-file)
            :collect `(:file ,static-file :mount ,(merge-pathnames (pathname static-app-prefix) (pathname (subseq (namestring static-file) (length static-path))))))))

(defun serve-file (params)
  (format nil "URI: ~A~%" (lack.request:request-uri ningle:*request*)))

;; (let ((content-type (barghest/utils/io:guess-mime-type path))) "Testing")

(defun prepare-static-routes (project app)
  (dolist (static-file-data (barghest/static:generate-static-urls project app))
    (push (barghest/routes:path (getf static-file-data :mount) #'serve-file :method :GET) patterns)))
