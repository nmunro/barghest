(defpackage barghest/tags
  (:use :cl)
  (:export #:static
           #:secondof))

(in-package barghest/tags)

(defun get-type (x)
  (etypecase x
    (string x)
    (number x)
    (symbol (djula::resolve-variable-phrase (list x)))))

;; This now works with variables as well as strings!
(djula:def-tag-compiler :static (path)
  (lambda (stream)
    (multiple-value-bind (val val-error)
        (get-type path)
      (cond
        (val
         (format stream "~A/~A" (getf (envy:config :malaga/settings) :static-url) val))

        (val-error
         (format stream "~A" val-error))))))

;; (djula:def-tag-compiler :url (name &rest kws &key &allow-other-keys)
;;   (format t "Handling: ~A~%" (getf barghest/routes:+urls+ name))
;;   (if (not kws)
;;     (lambda (stream) (format stream "~A" (getf barghest/routes:+urls+ name))))
;;     (let ((split-route (str:split "/" (getf barghest/routes:+urls+ name))))
;;         (dolist (pair (barghest/utils/tron:split-into-pairs kws))
;;           (alexandria:when-let (index (find-parameter (car pair) split-route))
;;             (typecase (cadr pair)
;;               (string
;;                 (setf (nth index split-route) (cadr pair)))

;;               (integer
;;                 (setf (nth index split-route) (format nil "~A" (cadr pair))))

;;               (keyword
;;                 (format t "Handling keyword: ~A -> ~A~%" (cadr pair) (get-template-variable (cadr pair)))
;;                 (format t "Not sure how to handle this yet~%")
;;                 "")

;;               (t
;;                 ""))))
;;         (str:join "/" split-route)
;;         (lambda (stream) (format stream (str:join "/" split-route)))))

(let ((a nil)
      (b 2))
  (cond
    (a
     "A is selected")

    (b
     "B is selected")))
