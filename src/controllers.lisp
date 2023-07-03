(defpackage barghest/controllers
  (:use :cl)
  (:shadow #:random
           #:delete
           #:get
           #:search
           #:filter
           #:first
           #:last)
  (:export #:model
           #:controller
           #:create
           #:first
           #:last
           #:get
           #:get-or-create
           #:get-object-or-default
           #:all
           #:random
           #:delete
           #:search))

(in-package barghest/controllers)

(defclass controller ()
  ((model :initarg :model :initform (error "Must provide a model") :reader model)))

(defgeneric all (controller)
  (:documentation "Returns all records"))

(defgeneric first (controller)
  (:documentation "Returns the row with the lowest ID"))

(defgeneric last (controller)
  (:documentation "Returns the row with the highest ID"))

(defgeneric get (controller &rest kws &key &allow-other-keys)
  (:documentation "Returns a single record matching the specified conditions, raises a multiple-items error if more than one match, raises a not-found error if no matches"))

(defgeneric random (controller &key exclude)
  (:documentation "Returns a random record"))

(defgeneric create (controller &rest kws &key &allow-other-keys)
  (:documentation "Creates an object"))

(defgeneric get-or-create (controller &rest kws &key &allow-other-keys)
  (:documentation "Gets an object or creates it if missing"))

(defgeneric delete (controller &rest kws &key &allow-other-keys)
  (:documentation "Deletes an object"))

(defmethod all ((controller controller))
  (mito:select-dao (model controller)))

(defmethod first ((controller controller))
  (car (mito:select-dao (model controller) (sxql:order-by (:ASC :id)) (sxql:limit 1))))

(defmethod last ((controller controller))
  (car (mito:select-dao (model controller) (sxql:order-by (:DESC :id)) (sxql:limit 1))))

(defmethod get ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:find-dao (cons (model controller) kws)))

(defmethod create ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:create-dao (cons (model controller) kws)))

(defmethod delete ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:delete-by-values (cons (model controller) kws)))

(defmethod get-or-create ((controller controller) &rest kws &key &allow-other-keys)
  (alexandria:if-let (obj (apply #'mito:find-dao (cons (model controller) kws)))
      (return-from get-or-create (values obj nil))
      (return-from get-or-create (values (apply #'create (cons controller kws)) t))))

(defmethod get-object-or-default ((controller controller) &rest kws &key (default nil) &allow-other-keys)
  (let ((default (getf kws :default)))
    (remf kws :default)
    (handler-case
        (apply #'get (cons controller kws))
      (error () default))))
