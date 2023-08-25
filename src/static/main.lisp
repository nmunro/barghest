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
(defparameter files (make-hash-table :test #'equal))

(defun build-path-for-app (app resource-type)
  (pathname (ppath:join "src" app resource-type app)))

(defun build-static-path-for-app (project app)
  (asdf:system-relative-pathname project (build-path-for-app app "static")))

(defun get-static-files (project app)
  (barghest/utils/io:list-files (build-static-path-for-app project app)))

(defun generate-static-urls (project app)
  (let ((static-path (namestring (build-static-path-for-app project app)))
        (static-app-prefix (format nil "~A~A" (pathname app) ppath.details.constants:+sep-string+)))
    (loop :for static-file :in (get-static-files project app)
          :unless (ppath:isdir static-file)
            :collect `(:file ,static-file
                       :mount ,(merge-pathnames (pathname static-app-prefix) (pathname (subseq (namestring static-file) (length static-path))))))))

(defun serve-file (params)
  (let* ((mount (lack.request:request-uri ningle:*request*))
         (content-type (barghest/utils/io:guess-mime-type (gethash mount files))))
    (setf (lack.response:response-headers ningle:*response*)
      (append (lack.response:response-headers ningle:*response*) (list :content-type content-type)))
    (if (str:starts-with-p "text/" content-type)
        (alexandria:read-file-into-string (gethash mount files))
        (alexandria:read-file-into-byte-vector (gethash mount files)))))

(defun prepare-static-routes (project app prefix)
  (dolist (static-file-data (generate-static-urls project app))
    (setf (gethash (ppath:join prefix (subseq (namestring (getf static-file-data :mount)) 1)) files) (getf static-file-data :file))
    (push (barghest/routes:path (getf static-file-data :mount) #'serve-file :method :GET) patterns)))
