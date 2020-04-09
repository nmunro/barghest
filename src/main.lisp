(defpackage barghest
  (:use :cl)
  (:export #:serve
           #:hello-world))
(in-package :barghest)

(defparameter *address* "127.0.0.1")
(defparameter *port* 8080)

(defun get-datetime ()
  (multiple-value-bind
    (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (declare (ignore day-of-week))
    (declare (ignore dst-p))
    (format nil "~2,'0d/~2,'0d/~A ~2,'0d:~2,'0d:~2,'0d (UTC~@d)" date month year hour minute second (- tz))))

(defun http-char (c1 c2 &optional (default #\Space))
  "Return the code char of the parsed integer, or the default parameter"
  (let ((code (parse-integer (coerce (list c1 c2) 'string) :radix 16 :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  "Take a string and break it up and recursively break it down into http chars"
  (labels
    ((f (lst)
       (when lst
         (case (car lst)
           (#\% (cons (http-char (cadr lst) (caddr lst)) (f (cdddr lst))))
           (#\+ (cons #\Space (f (cdr lst))))
           (otherwise (cons (car lst) (f (cdr lst))))))))
      (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s (+ 2 (position #\Space s)) (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(defun serve (app)
  (usocket:with-server-socket (server-socket (usocket:socket-listen *address* *port*))
    (format t "Starting server on: ~A:~A~%" *address* *port*)
    (unwind-protect
      ; protect form
      (loop (usocket:with-connected-socket (server-connection (usocket:socket-accept server-socket))
        (with-open-stream (stream (usocket:socket-stream server-connection))
          (let* ((url    (parse-url (read-line stream)))
                 (path   (car url))
                 (header (get-header stream))
                 (params (append (cdr url) (get-content-params stream header))))
              (format t "~A -> REQUEST: Path: /~A~%" (get-datetime) path)
              (funcall app path header params stream)))))

      ; cleanup form
      (format t "Server shut down!~%")
      (usocket:socket-close server-socket))))

(defun hello-world (path header params stream)
  (declare (ignore header)) ; Ignore this for now
  (format t "Test params: ~{~A~^, ~}~%" params)
  (format t "Type: ~A~%" (type-of params))
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (format t "Name: ~A~%" name)
        (if (not name)
            (format stream (format-data "<html><form>What is your name?<input name='name' /><form></html>"))
            (format stream (format-data (format nil "<html>Nice to meet you, ~A!</html>" (cdr name))))))

      (format stream (error-404 path))))

(defun error-404 (page)
  (let* ((msg (format nil "Sorry... I don't know the page: \"~A\"" page))
         (content-length (length msg)))
    (format nil "HTTP/1.1 404 Not Found~%Content-Type: text/html~%Content-Length: ~A~%~%~A" content-length msg)))

(defun format-data (content)
  (let ((content-length (length content)))
    (format nil "HTTP/1.1 200 OK~%Content-Type: text/html~%Content-Length: ~A~%~%~A" content-length content)))
