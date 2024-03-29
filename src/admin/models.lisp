(defpackage barghest/admin/models
  (:use :cl)
  (:export #:user
           #:role
           #:permissions))

(in-package barghest/admin/models)

(mito:deftable user ()
  ((name     :col-type (:varchar 255))
   (email    :col-type (:varchar 255))
   (password :col-type (or (:varchar 512) :null)))
  (:unique-keys name))

(mito:deftable role ()
  ((name :col-type (:varchar 255)))
  (:unique-keys name))

(mito:deftable permissions ()
  ((user :col-type user)
   (role :col-type role))
  (:unique-keys (user role)))
