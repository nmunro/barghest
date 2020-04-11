(defpackage barghest
  (:use :cl
        :barghest.request)
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

(defun serve (app)
  (usocket:with-server-socket (server-socket (usocket:socket-listen *address* *port*))
    (format t "Starting server on: ~A:~A~%" *address* *port*)
    (unwind-protect
      ; protect form
      (loop (usocket:with-connected-socket (server-connection (usocket:socket-accept server-socket))
        (with-open-stream (stream (usocket:socket-stream server-connection))
          (let ((req (barghest.request:make-request stream)))
              (funcall app req stream)))))

      ; cleanup form
      (format t "Server shut down!~%")
      (usocket:socket-close server-socket))))

(defun hello-world (req stream)
  (format t "Hello-World: ~A -> ~A~%" (get-datetime) req)

  (if (equal (barghest.request:path req) "greeting")
    (if (not (gethash "name" (barghest.request:params req)))
      (format stream (format-data "<html><form>What is your name?<input name='name' /><form></html>"))
      (format stream (format-data (format nil "<html>Nice to meet you, ~A!</html>" (gethash "NAME" (barghest.request:params req))))))

    (format stream (error-404 (barghest.request:path req)))))

(defun error-404 (page)
  (let* ((msg (format nil "Sorry... I don't know the page: \"~A\"" page))
         (content-length (length msg)))
    (format nil "HTTP/1.1 404 Not Found~%Content-Type: text/html~%Content-Length: ~A~%~%~A" content-length msg)))

(defun format-data (content)
  (let ((content-length (length content)))
    (format nil "HTTP/1.1 200 OK~%Content-Type: text/html~%Content-Length: ~A~%~%~A" content-length content)))
