(defpackage barghest/auth/controllers
  (:use :cl)
  (:shadow #:search)
  (:export #:model
           #:search
           #:user-permission
           #:+user+
           #:+role+
           #:+permission+))

(in-package barghest/auth/controllers)

(defclass user (barghest/controllers:controller)
  ((model :initarg :model :initform 'barghest/auth/models:user :reader model)))

(defclass role (barghest/controllers:controller)
  ((model :initarg :model :initform 'barghest/auth/models:role :reader model)))

(defclass permission (barghest/controllers:controller)
  ((model :initarg :model :initform 'barghest/auth/models:permission :reader model)))

(defmethod user-permission ((controller permission) user)
  (mito:select-dao 'barghest/auth/models:permission
                   (mito:includes 'barghest/auth/models:user)
                   (mito:includes 'barghest/auth/models:role)
                   (sxql:where (:= :user user))))

(defmethod search ((controller permission) &key (search string) (player nil) (paginate nil) (offset 0) (limit 500))
  (declare (ignore search paginate offset limit))
    (mito:select-dao (model controller)
            (mito:includes 'barghest/auth/models:user)
            (mito:includes 'barghest/auth/models:role)
            (sxql:inner-join :user :on (:= :user.id :permission.user_id))
            (sxql:inner-join :role :on (:= :role.id :permission.role_id))
            (sxql:where (:= :user player))))

(defvar +user+ (make-instance 'user :model 'barghest/auth/models:user))
(defvar +role+ (make-instance 'role :model 'barghest/auth/models:role))
(defvar +permission+ (make-instance 'permission :model 'barghest/auth/models:permission))
