(defpackage barghest/auth/views
  (:use :cl)
  (:export #:render-login
           #:process-login
           #:logout))

(in-package barghest/auth/views)

(defun render-login (params)
  (barghest/http:render "auth/login.html" :next-url (barghest/http:get-next-url)))

(defun process-login (params)
  (let ((username (cdr (assoc "username" params :test #'equal)))
        (password (cdr (assoc "password" params :test #'equal))))
    (handler-case (cerberus:login :user username :password password)
        (cerberus:invalid-user (err)
            (return-from process-login (barghest/http:render
                                        "auth/login.html"
                                        :msg (cerberus:msg err)
                                        :next-url (barghest/http:get-next-url))))

          (cerberus:invalid-password (err)
            (return-from process-login (barghest/http:render
                                        "auth/login.html"
                                        :msg (cerberus:msg err)
                                        :next-url (barghest/http:get-next-url))))))

      (alexandria:if-let (next-url (cdr (assoc "next" params :test #'equal)))
        (return-from process-login (barghest/http:redirect (barghest/http:get-next-url)))
        (return-from process-login (barghest/http:redirect "/admin/"))))

(defun logout (params)
  (when (cerberus:user-name)
    (cerberus:logout))
  (barghest/http:redirect "/"))
