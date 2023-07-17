(defpackage barghest/auth
  (:use :cl)
  (:export #:set-password))

(in-package barghest/auth)

(defun set-password (username password)
  (let ((password-hash (cl-pass:hash password :type :pbkdf2-sha256 :iterations 10000))
        (user (barghest/controllers:get barghest/admin/controllers:+user+ :name username)))
    (setf (slot-value user 'barghest/admin/models:password) password-hash)
    (mito:save-dao user)))
