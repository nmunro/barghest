(defpackage barghest
  (:use :cl
        :barghest.request)
  (:export #:serve
           #:hello-world))
(in-package :barghest)

(defparameter *address* "127.0.0.1")
(defparameter *port* 8080)

(defun serve (app)
  (usocket:with-server-socket (server-socket (usocket:socket-listen *address* *port*))
    (format t "Starting server on: ~A:~A~%" *address* *port*)
    (unwind-protect
      ; protect form
      (loop (usocket:with-connected-socket (server-connection (usocket:socket-accept server-socket))
        (with-open-stream (stream (usocket:socket-stream server-connection))
          ; Request and response objects are created here
          (let ((req (barghest.request:make-request stream))
                (res (barghest.response:make-response stream)))
              (funcall app req res)))))

      ; cleanup form
      (format t "Server shut down!~%")
      (usocket:socket-close server-socket))))

(defun hello-world (req res)
  (format t "Hello-World: ~A -> ~A~%" (barghest.datetime:datetime) req)

  (if (equal (barghest.request:path req) "greeting")
    (if (not (gethash "name" (barghest.request:args req)))
      (barghest.response:send-response res "<html><form>What is your name?<input name='name' /><form></html>")
      (barghest.response:send-response res (format nil "<html>Nice to meet you, ~A!</html>" (gethash "NAME" (barghest.request:args req)))))

    (error-404 req res)))

(defun error-404 (req res)
  (setf (barghest.response:status-code res) (barghest.status-codes:make-status-code :404))
  (barghest.response:send-response res (format nil "Sorry... I don't know the page: \"~A\"" (barghest.request:path req))))
