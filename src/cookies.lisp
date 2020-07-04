(defpackage barghest.cookies
  (:use :cl)
  (:export #:make-cookie))
(in-package :barghest.cookies)

(defclass cookie ()
  ((name      :initarg :name      :initform "" :accessor name)
   (value     :initarg :value     :initform "" :accessor value)
   (expires   :initarg :expires   :initform "" :accessor expires)
   (max-age   :initarg :max-age   :initform "" :accessor max-age)
   (domain    :initarg :domain    :initform "" :accessor domain)
   (path      :initarg :path      :initform "" :accessor path)
   (secure    :initarg :secure    :initform "" :accessor secure)
   (httponly  :initarg :httponly  :initform "" :accessor httponly)
   (same-site :initarg :same-site :initform "" :accessor same-site)))

(defmethod print-object ((object cookie) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~A" (name object) (value object))))

(defun make-keyword (name)
  (values (intern (string-upcase name) "KEYWORD")))

(defun make-cookie (cookie-string)
  (let ((cookie-data (parse-cookie cookie-string)))
    (make-instance 'cookie
                   :name      (getf cookie-data :name)
                   :value     (getf cookie-data :value)
                   :expires   (getf cookie-data :expires)
                   :max-age   (getf cookie-data :max-age)
                   :domain    (getf cookie-data :domain)
                   :path      (getf cookie-data :path)
                   :secure    (getf cookie-data :secure)
                   :httponly  (getf cookie-data :httponly)
                   :same-site (getf cookie-data :same-site))))

(defun parse-cookie (s)
  ; Need to get the first k=v pair and store that as name and value
  (let* ((data (mapcar #'(lambda (x) (string-trim '(#\Space) x)) (uiop:split-string s :separator ";")))
       (parsed-data (mapcar #'(lambda (s) (uiop:split-string s :separator "=")) data))
       (rtn '()))

    ; sort out the cookie key and value first
    (push (first (first parsed-data)) rtn)
    (push (make-keyword "name") rtn)
    (push (second (first parsed-data)) rtn)
    (push (make-keyword "value") rtn)

    ; then process and add whatever remains
    (dolist (item (rest parsed-data))
      (if (eq nil (second item))
          (push t rtn)
          (push `,(second item) rtn))

        (push (make-keyword (first item)) rtn))

    rtn))

(let ((cookie (make-cookie "mykey=myvalue; expires=Mon, 17-Jul-2017 16:06:00 GMT; Max-Age=31449600; Path=/; secure; HttpOnly")))
  (format t "~A: ~A (~A)~%" (name cookie) (value cookie) (httponly cookie)))
