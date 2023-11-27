(defpackage barghest/auth
  (:use :cl)
  (:export #:create-user
           #:set-password
           #:user-csrf-token
           #:user-p
           #:user-pass
           #:user-roles
           #:user-csrf-token))

(in-package barghest/auth)

(defun set-password (username password)
  (let ((password-hash (cl-pass:hash password :type :pbkdf2-sha256 :iterations 10000))
        (user (barghest/controllers:get barghest/auth/controllers:+user+ :name username)))
    (setf (slot-value user 'barghest/auth/models::password) password-hash)
    (mito:save-dao user)))

(defun create-user (&key (username nil username-p) (email nil email-p) (pass (barghest/crypt:make-user-password 16) pass-p) (roles (list "user")))
  (let* ((hash (cl-pass:hash pass :type :pbkdf2-sha256 :iterations 10000))
         (user (barghest/controllers:get-or-create barghest/auth/controllers:+user+ :name username :password hash :email email)))
    (dolist (role roles)
      (barghest/controllers:get-or-create
         barghest/auth/controllers:+permission+
         :user user
         :role (barghest/controllers:get-or-create barghest/auth/controllers:+role+ :name role)))
    user))

(defun user-p (user)
  (barghest/controllers:get barghest/auth/controllers:+user+ :name user))

(defun user-pass (user)
  (slot-value (barghest/controllers:get barghest/auth/controllers:+user+ :name user) 'barghest/auth/models::password))

(defun user-roles (user)
  (loop :for role
        :in (barghest/auth/controllers:search barghest/auth/controllers:+permission+ :player (barghest/controllers:get barghest/auth/controllers:+user+ :name user))
        :collect (slot-value (slot-value role 'barghest/auth/models:role) 'barghest/auth/models::name)))

(defun user-csrf-token (user)
  (let ((s (format nil "~A~A" user (get-universal-time))))
    (car (last (uiop:split-string (cl-pass:hash s :type :pbkdf2-sha256 :iterations 10000) :separator '(#\$))))))
