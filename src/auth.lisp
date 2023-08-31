(defpackage barghest/auth
  (:use :cl)
  (:export #:create-user
           #:set-password))

(in-package barghest/auth)

(defun set-password (username password)
  (let ((password-hash (cl-pass:hash password :type :pbkdf2-sha256 :iterations 10000))
        (user (barghest/controllers:get barghest/admin/controllers:+user+ :name username)))
    (setf (slot-value user 'barghest/admin/models::password) password-hash)
    (mito:save-dao user)))

(defun create-user (&key (username nil username-p) (email nil email-p))
  (unless (and username-p email-p)
    (error "Both username and email *must* be provided"))

  (let* ((pass (barghest/crypt:make-user-password 16))
         (hash (cl-pass:hash pass :type :pbkdf2-sha256 :iterations 10000))
         (user (barghest/controllers:get-or-create barghest/admin/controllers:+user+ :name username :password hash :email email)))
    (dolist (role (list "admin" "user"))
        (barghest/controllers:get-or-create
         barghest/admin/controllers:+permissions+
         :user user
         :role (barghest/controllers:get-or-create barghest/admin/controllers:+role+ :name role)))
    user))
