(defpackage barghest/admin/urls
  (:use :cl)
  (:export #:patterns))

(in-package barghest/admin/urls)

(defparameter patterns (list
  (barghest/routes:path "/" #'barghest/admin/views:admin :method :GET :name :admin-index)
  (barghest/routes:path ":object/" #'barghest/admin/views:get :method :GET :name :list-objects)
  (barghest/routes:path ":object/" #'barghest/admin/views:save :method :POST :name :save-object)
  (barghest/routes:path ":object/add/" #'barghest/admin/views:add :method :GET :name :add-object)
  (barghest/routes:path ":object/:id/" #'barghest/admin/views:get :method :GET :name :get-object)
  (barghest/routes:path ":object/:id/" #'barghest/admin/views:save :method :POST :name :update-object)))
