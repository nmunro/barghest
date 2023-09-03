(defpackage barghest/auth/controllers
  (:use :cl)
  (:shadow #:search)
  (:export #:model
           #:search
           #:stale-users
           #:user-permissions
           #:+user+
           #:+role+
           #:+permissions+))

(in-package barghest/auth/controllers)

(defclass user (barghest/controllers:controller)
  ((model :initarg :model :initform 'barghest/auth/models:user :reader model)))

(defclass role (barghest/controllers:controller)
  ((model :initarg :model :initform 'barghest/auth/models:role :reader model)))

(defclass permissions (barghest/controllers:controller)
  ((model :initarg :model :initform 'barghest/auth/models:permissions :reader model)))

(defmethod user-permissions ((controller permissions) user)
  (mito:select-dao 'barghest/auth/models:permissions
                   (mito:includes 'barghest/auth/models:user)
                   (mito:includes 'barghest/auth/models:role)
                   (sxql:where (:= :user user))))

(defmethod stale-users ((controller user) users)
  (mito:select-dao (model controller) (sxql:where (:not-in :name users))))

(defmethod search ((controller permissions) &key (search string) (player nil) (paginate nil) (offset 0) (limit 500))
  (declare (ignore search paginate offset limit))
    (mito:select-dao (model controller)
            (mito:includes 'barghest/auth/models:user)
            (mito:includes 'barghest/auth/models:role)
            (sxql:inner-join :user :on (:= :user.id :permissions.user_id))
            (sxql:inner-join :role :on (:= :role.id :permissions.role_id))
            (sxql:where (:= :user player))))

(defvar +user+ (make-instance 'user :model 'barghest/auth/models:user))
(defvar +role+ (make-instance 'role :model 'barghest/auth/models:role))
(defvar +permissions+ (make-instance 'permissions :model 'barghest/auth/models:permissions))
