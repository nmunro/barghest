(defpackage barghest
  (:use :cl)
  (:import-from :barghest.datetime     :datetime-now)
  (:import-from :barghest.http         :make-status-code)
  (:import-from :barghest.request      :make-request)
  (:import-from :barghest.response     :render)
  (:import-from :barghest.response     :make-response)
  (:export #:serve
           #:hello-world))
(in-package :barghest)

(defun serve (app &key (address "127.0.0.1") (port 8080))
  (usocket:with-server-socket (server-socket (usocket:socket-listen address port))
    (format t "Starting server on: ~A:~A~%" address port)
    (unwind-protect
      ; protect form
      (loop (usocket:with-connected-socket (server-connection (usocket:socket-accept server-socket))
        (with-open-stream (stream (usocket:socket-stream server-connection))
          ; Request and response objects are created here
          (let ((req (make-request  stream))
                (res (make-response stream)))
              (funcall app req res)))))

      ; cleanup form
      (format t "Server shut down!~%")
      (usocket:socket-close server-socket))))

(defun hello-world (req res)
  (format t "Hello-World: ~A -> ~A~%" (datetime-now) req)

  (if (equal (barghest.request:path req) "greeting")
    (if (not (gethash "name" (barghest.request:args req)))
      (render res "<html><form>What is your name?<input name='name' /><form></html>")
      (render res (format nil "<html>Nice to meet you, ~A!</html>" (gethash "name" (barghest.request:args req)))))

    (request-abort :404 req res)))

(defun request-abort (status req res)
  (setf (barghest.response:status res) (make-status-code status))
  (render res (format nil "Sorry... I don't know the page: \"~A\"" (barghest.request:path req))))
