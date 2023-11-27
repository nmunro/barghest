(defpackage barghest/auth/models
  (:use :cl)
  (:export #:user
           #:role
           #:permission))

(in-package barghest/auth/models)

(mito:deftable user ()
  ((name     :col-type (:varchar 255))
   (email    :col-type (:varchar 255))
   (password :col-type (or (:varchar 512) :null)))
  (:unique-keys email))

(mito:deftable role ()
  ((name :col-type (:varchar 255)))
  (:unique-keys name))

(mito:deftable permission ()
  ((user :col-type user)
   (role :col-type role))
  (:unique-keys (user role)))
