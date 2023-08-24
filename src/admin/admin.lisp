(defpackage barghest/admin/admin
  (:use :cl))

(in-package barghest/admin/admin)

(barghest/app:register-model 'barghest/admin/models:user)
(barghest/app:register-model 'barghest/admin/models:role)
