(defpackage barghest/crypt
  (:use :cl)
  (:export #:make-user-password
           #:validate
           #:generate-password
           #:is-email))

(in-package barghest/crypt)

(defun get-char-range (start end)
  (loop :for x :from (char-code start) :to (char-code end) :collect (code-char x)))

;; Code taken from: https://rosettacode.org/wiki/Password_generator#Common_Lisp
(defvar *lowercase* (get-char-range #\a #\z))

(defvar *uppercase* (get-char-range #\A #\Z))

(defvar *numbers* (get-char-range #\0 #\9))

(defvar *special-characters* '(#\! #\\ #\# #\$ #\% #\& #\' #\( #\) #\*
                              #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\>
                              #\? #\@ #\[ #\] #\^ #\_ #\{ #\| #\} #\~))

(defvar *similar-characters* '(#\I #\l #\1 #\| #\O #\0 #\5 #\S #\2 #\Z))

(defun make-readable (s)
  (remove-if (lambda (x) (member x *similar-characters*)) s))

(defun shuffle-list (input-list)
  (loop with l = (length input-list)
     for i below l
     do (rotatef (nth i input-list)
         (nth (random l) input-list)))
  input-list)

(defun generate-password (len human-readable)
  (let*
    ((upper (if human-readable (make-readable *uppercase*) *uppercase*))
     (lower (if human-readable (make-readable *lowercase*) *lowercase*))
     (number (if human-readable (make-readable *numbers*) *numbers*))
     (special (if human-readable (make-readable *special-characters*) *special-characters*))
     (character-groups (list upper lower number special))
     (initial-password (reduce (lambda (acc x)
      (cons (nth (random (length x)) x) acc))
        character-groups :initial-value NIL)))

    (coerce (shuffle-list (reduce (lambda (acc x)
      (declare (ignore x))
      (let ((group (nth (random (length character-groups)) character-groups)))
        (cons (nth (random (length group)) group) acc)))
      (make-list (- len 4)) :initial-value initial-password)) 'string)))

(defun make-user-password (len &optional human-readable)
  (if (< len 4)
    (print "Length must be at least 4~%")
    (loop :for pw = (generate-password len human-readable)
          :until (validate pw pw)
          :collect pw)))

(defun get-special-chars (chars)
  (loop :for char :in chars :collect (code-char (char-code char))))

(defun contains-chars (pw chars)
  (dolist (char chars)
    (when (str:containsp (string char) pw)
      (return-from contains-chars t))))

(defun match (pw1 pw2)
  (string= pw1 pw2))

(defun min-length (pw &key (min-length 8))
  (>= (length pw) min-length))

(defun max-length (pw &key (max-length 32))
  (<= (length pw) max-length))

(defun not-common (pw)
  (let ((path (asdf:system-relative-pathname "barghest" (ppath:join "src" "auth" "common-passwords.txt"))))
    (not (member pw (uiop:read-file-lines path) :test #'equal))))

(defmethod is-email ((email string))
  (let ((scanner (ppcre:create-scanner "^[A-Za-z0-9+_.-]+@(.+)$")))
    (ppcre:scan scanner email)))

(defun contains-number (pw)
  (contains-chars pw *numbers*))

(defun contains-lower-case-letter (pw)
  (contains-chars pw *lowercase*))

(defun contains-upper-case-letter (pw)
  (contains-chars pw *uppercase*))

(defun contains-special-chars (pw special-chars)
  (contains-chars pw (get-special-chars special-chars)))

(defun validate (pw1 pw2 &key (min-length 8) (max-length 32) (special-chars *special-characters*))
  (and (match pw1 pw2)
      (min-length pw1 :min-length min-length)
      (max-length pw1 :max-length max-length)
      (not-common pw1)
      (contains-number pw1)
      (contains-lower-case-letter pw1)
      (contains-upper-case-letter pw1)
      (contains-special-chars pw1 special-chars)))
