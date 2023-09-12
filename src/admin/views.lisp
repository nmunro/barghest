(defpackage barghest/admin/views
  (:use :cl)
  (:shadow #:get)
  (:import-from :barghest/auth/controllers
                :+user+
                :+role+
                :+permissions+)
  (:export #:add
           #:admin
           #:get
           #:get
           #:save
           #:login
           #:logout))

(in-package barghest/admin/views)

(defun load-controller (name package)
  (let ((model (find-class (read-from-string (format nil "barghest/~A/models:~A" package name))))
        (controller (find-class (read-from-string (format nil "barghest/~A/controllers::~A" package name)))))
    (funcall #'make-instance controller :model model)))

(defun load-slot (name package)
  (read-from-string (format nil "barghest/~A/models:~A" package name)))

(defun clean-form (params)
    (setf params (remove (assoc "action" params :test #'equalp) params :test #'equal))
    (setf params (remove (assoc "csrf-token" params :test #'equalp) params :test #'equal))
    (setf params (remove (assoc :object params :test #'equal) params :test #'equal)))

(defun admin (params)
  (declare (ignore params))
  (cond
    ((cerberus:auth "admin")
      (return-from admin (barghest/http:render "admin/admin.html" :msg "Barghest Admin")))

    ((and (cerberus:logged-in-p) (not (cerberus:auth "admin")))
     (return-from admin (barghest/http:forbidden "admin/403.html" :msg "You are not authorized to view this page")))

    ((barghest/http:get-next-url)
     (return-from admin (barghest/http:redirect "/auth/login/" :next-url (barghest/http:get-next-url))))

    (t
     (return-from admin (barghest/http:redirect "/auth/login/")))))

(defgeneric create-object (object kws)
  (:documentation "Create an object"))

(defgeneric get-object (object kws)
  (:documentation "Gets an object"))

(defgeneric save-object (object obj kws)
  (:documentation "Save an object"))

(defgeneric delete-object (object obj)
  (:documentation "Delete an object"))

(defmethod create-object ((object (eql :user)) kws)
  (apply #'barghest/controllers:create (append `(,(load-controller "user" "auth")) kws)))

(defmethod create-object ((object (eql :role)) kws)
  (apply #'barghest/controllers:create (append `(,(load-controller "role" "auth")) kws)))

(defmethod get-object ((object (eql :user)) id)
  (flet ((get-role (role) `(:role ,role :selected ,(cerberus:auth (slot-value role 'barghest/auth/models::name)))))
    (let* ((user (barghest/controllers:get (load-controller "user" "auth") :id id))
           (permissions (barghest/auth/controllers:user-permissions (load-controller "permissions" "auth") user)))
        (barghest/http:render
            "admin/user.html"
            :item user
            :csrf-token (cerberus:csrf-token)
            :roles (loop :for role :in (barghest/controllers:all (load-controller "role" "auth")) :collect (get-role role))))))

(defmethod get-object ((object (eql :role)) id)
  (barghest/http:render
    "admin/role.html"
    :item (barghest/controllers:get (load-controller "role" "auth") :id id)
    :csrf-token (cerberus:csrf-token)))

(defmethod get-object ((object (eql :users)) id)
  (declare (ignore id))
  (barghest/http:render "admin/list-user.html" :items (barghest/controllers:all (load-controller "user" "auth"))))

(defmethod get-object ((object (eql :roles)) id)
  (declare (ignore id))
  (barghest/http:render "admin/list-role.html" :items (barghest/controllers:all (load-controller "role" "auth"))))

(defmethod save-object ((object (eql :user)) obj kws)
  (dolist (kw (loop :for (k v) :on kws :by #'cddr :collect k))
    (unless (eq kw :permission)
      (setf (slot-value obj (load-slot kw "auth")) (getf kws kw))
      (setf kws (cddr kws))))
  (mito:save-dao obj)
  (setf kws (remove :permission kws))

  (dolist (perm (barghest/auth/controllers:user-permissions (load-controller "permissions" "auth") obj))
    (barghest/controllers:delete (load-controller "permissions" "auth") :id (mito:object-id perm)))

  (dolist (kw kws)
    (barghest/controllers:get-or-create
     (load-controller "permissions" "auth")
     :user obj
     :role (barghest/controllers:get (load-controller "role" "auth") :name kw))))

(defmethod save-object ((object (eql :role)) obj kws)
  (dolist (kw (loop :for (k v) :on kws :by #'cddr :collect k))
    (setf (slot-value obj (load-slot kw "auth")) (getf kws kw)))
  (mito:save-dao obj))

(defmethod delete-object ((object (eql :user)) obj)
  (apply #'barghest/controllers:delete `(,(load-controller "user" "auth") :id ,(mito:object-id obj))))

(defmethod delete-object ((object (eql :role)) obj)
  (apply #'barghest/controllers:delete `(,(load-controller "role" "auth") :id ,(mito:object-id obj))))

(defun process-object (action object fields)
  (let ((controller (load-controller object "auth"))
        (id (cdr (assoc :id fields :test #'equal)))
        (kws (barghest/utils/tron:alist-to-plist (remove (assoc :id fields :test #'equalp) fields :test #'equal))))
    (cond
      ((equalp action "create")
         (return-from process-object (create-object (intern (string-upcase object) "KEYWORD") kws)))

      ((equalp action "save")
         (alexandria:when-let (obj (barghest/controllers:get controller :id id))
            (return-from process-object (save-object (intern (string-upcase object) "KEYWORD") obj kws))))

      ((equalp action "delete")
         (alexandria:when-let (obj (barghest/controllers:get controller :id id))
            (return-from process-object (delete-object (intern (string-upcase object) "KEYWORD") obj)))))))

(defun get (params)
  (cond
    ((and (cerberus:logged-in-p) (not (cerberus:auth "admin")))
     (return-from get (barghest/http:forbidden "admin/403.html" :msg "You are not authorized to view this page")))

    ((not (cerberus:auth "admin"))
     (return-from get (barghest/http:redirect "/admin/" :next-url (barghest/http:get-current-url))))

    ((and (equalp (cdr (assoc :object params :test #'equal)) "user") (cdr (assoc :id params :test #'equal)))
      (get-object :user (cdr (assoc :id params :test #'equal))))

    ((equalp (cdr (assoc :object params :test #'equal)) "user")
      (get-object :users :none))

    ((and (equalp (cdr (assoc :object params :test #'equal)) "role") (cdr (assoc :id params :test #'equal)))
      (get-object :role (cdr (assoc :id params :test #'equal))))

    ((equalp (cdr (assoc :object params :test #'equal)) "role")
      (get-object :roles :none))))

(defun save (params)
  ; @TODO: A generic save that recovers from duplicate errors needs to be written
  (let* ((object (cdr (assoc :object params :test #'equal)))
         (item (cdr (assoc "name" params :test #'equal)))
         (action (cdr (assoc "action" params :test #'equal)))
         (csrf-token (cdr (assoc "csrf-token" params :test #'equal))))
    (unless (string= csrf-token (cerberus:csrf-token))
      (return-from save (barghest/http:forbidden "403.html" :msg "CSRF Token missing")))

    (alexandria:if-let (data (process-object action object (clean-form params)))
        (return-from save (barghest/http:redirect (format nil "/admin/~A/~A" object (mito:object-id data))))
        (return-from save (barghest/http:redirect (format nil "/admin/~A" object))))))

(defun add (params)
  (let ((object (cdr (assoc :object params :test #'equal))))
    (barghest/http:render (format nil "admin/~A.html" object) :csrf-token (cerberus:csrf-token))))
