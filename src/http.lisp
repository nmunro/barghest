(defpackage barghest/http
  (:use :cl)
  (:export #:get-next-url
           #:auth-required
           #:not-allowed
           #:redirect
           #:forbidden
           #:render))

(in-package barghest/http)

(defun render (template &rest kws &key &allow-other-keys)
  (let ((template (djula:compile-template* template)))
    (apply #'djula:render-template* (append `(,template nil) kws))))

(define-condition redirect-error (error)
  ((message :initarg :message :initform "Args mismatched" :reader message)))

(defun auth-required (fn &key permissions)
  (lambda (params)
    (if (cerberus:auth permissions)
      (funcall fn params)
      (redirect "/admin/login" :next-url (get-current-url)))))

(defun redirect (url &key next-url return-url)
  (cond
    ((and next-url return-url)
     (error 'redirect-error :message "Can't use next-url and return together"))

    ((and next-url (string= next-url ""))
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list "Location" (format nil "~A" url)))))

    ((and return-url (string= return-url ""))
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list "Location" (format nil "~A" url)))))

    ((and return-url (str:contains? "?" url))
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list "Location" (format nil "~A&next=~A" url (lack.request:request-uri ningle:*request*))))))

    ((and return-url (not (str:contains? "?" url)))
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list "Location" (format nil "~A?next=~A" url (lack.request:request-uri ningle:*request*))))))

    ((and next-url (str:contains? "?" url))
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list "Location" (format nil "~A&next=~A" url next-url)))))

    ((and next-url (not (str:contains? "?" url)))
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list "Location" (format nil "~A?next=~A" url next-url)))))

    (t
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list "Location" url)))))

  (setf (lack.response:response-status ningle:*response*) "303"))

(defun forbidden (template &key msg)
  (setf (lack.response:response-status ningle:*response*) "403")
  (render template :msg msg))

(defun not-allowed (template &key msg)
  (setf (lack.response:response-status ningle:*response*) "405")
  (render template :msg "Method Not Allowed"))

(defun get-current-url ()
  (lack.request:request-uri ningle:*request*))

(defun get-next-url ()
  (format nil "~A" (or (cdr (assoc "next" (lack.request:request-query-parameters ningle:*request*) :test #'equal)) "")))
