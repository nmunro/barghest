(defpackage barghest/http
  (:use :cl)
  (:export #:get-next-url
           #:auth-required
           #:not-allowed
           #:redirect
           #:forbidden
           #:get-current-url
           #:get-next-url
           #:render))

(in-package barghest/http)

(defun render (template &rest kws &key &allow-other-keys)
  (let ((template (djula:compile-template* template)))
    (apply #'djula:render-template* (append `(,template nil) kws))))

(define-condition redirect-error (error)
  ((message :initarg :message :initform "Args mismatched" :reader message)))

(defun auth-required (fn &key permissions)
  (lambda (params)
    (cond
      ((and (cerberus:logged-in-p) (cerberus:auth permissions))
        (funcall fn params))

      ((cerberus:logged-in-p)
       (not-allowed "405.html" :msg "Not Allowed"))

      (t
       (redirect "/admin/login" :next-url (get-current-url))))))

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

(defun build-url-query-params (url next)
  (setf (quri:uri-query-params url) (remove-if (lambda (entry) (string= "next" entry)) (quri:uri-query-params url) :key #'car))
  (push (cons "next" next) (quri:uri-query-params url))
  (quri:uri-query-params url))

(defun get-args (url next)
  (loop :for arg :in (build-url-query-params url next)
        :if (string= (car arg) "next")
          :collect (format nil "~A=~A" (car arg) (cdr arg))
        :else
          :collect (format nil "~A=~A" (car arg) (quri:url-encode (cdr arg)))))

(defun remove-args-from-url (data)
  (multiple-value-bind (url start end)
      (quri:parse-query (format nil "~A" data))
    (declare (ignore end))
    (subseq (format nil "~A" url) 0 (1- start))))

(defun redirect-url (url next)
  (cond
    ((and (string/= "" next) (not (quri:uri-query-params url)))
     (return-from redirect-url (format nil "~A?next=~A" url next)))

    ((and (string= "" next) (not (quri:uri-query-params url)))
      (return-from redirect-url (format nil "~A" url)))

    ((and (string= "" next) (quri:uri-query-params url))
      (return-from redirect-url (format nil "~A?~{~A~^&~}" (remove-args-from-url url) (get-args url next))))

    ((and (string/= "" next) (quri:uri-query-params url))
      (return-from redirect-url (format nil "~A?~{~A~^&~}" (remove-args-from-url url) (get-args url next))))

    (t
      (return-from redirect-url (format nil "~A?~{~A~^&~}" url (get-args url))))))

(defun redirect (url &key (next-url ""))
  (setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*) (list "Location" (redirect-url (quri:uri url) next-url))))
  (setf (lack.response:response-status ningle:*response*) "303"))
