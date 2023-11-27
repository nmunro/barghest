(defpackage barghest/auth/views
  (:use :cl)
  (:export #:render-login
           #:process-login
           #:render-register
           #:process-register
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

(defun render-register (params)
  (barghest/http:render "auth/register.html" :next-url (barghest/http:get-next-url)))

(defun process-register (params)
  (let ((email (cdr (assoc "email" params :test #'equal)))
        (password (cdr (assoc "password" params :test #'equal)))
        (confirm (cdr (assoc "confirm-password" params :test #'equal))))
    (cond
      ((barghest/controllers:get barghest/auth/controllers:+user+ :email email)
       (return-from process-register
         (barghest/http:render "auth/register.html"
                               :next-url (barghest/http:get-next-url)
                               :errors (format nil "User ~A already exists" email))))

      ((string/= password confirm)
       (return-from process-register
         (barghest/http:render "auth/register.html"
                              :next-url (barghest/http:get-next-url)
                              :errors "Passwords do not match")))

      (t
        (let ((user (barghest/auth:create-user :username email :email email :pass password)))
          (handler-case (cerberus:login :user email :password password)
            (cerberus:invalid-user (err)
              (return-from process-register (barghest/http:redirect "/auth/register/")))

            (:no-error (c)
              (return-from process-register (barghest/http:redirect "/")))))))))

(defun logout (params)
  (when (cerberus:user-name)
    (cerberus:logout))
  (barghest/http:redirect "/"))
