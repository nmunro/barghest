(defpackage barghest
  (:use :cl)
  (:import-from :barghest.datetime     :datetime-now)
  (:import-from :barghest.http         :make-status-code)
  (:import-from :barghest.http         :client-error)
  (:import-from :barghest.http         :server-error)
  (:import-from :barghest.request      :make-request)
  (:import-from :barghest.response     :render)
  (:import-from :barghest.response     :render-error)
  (:import-from :barghest.response     :make-response)
  (:export #:serve
           #:hello-world))
(in-package :barghest)

; Load the djula templates here for now
(djula:add-template-directory (asdf:system-relative-pathname "barghest" "templates/"))

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
            (handler-case (funcall app req res)
              ; handle 4xx errors
              (client-error (err) (render-error err res))
              ; handle 5xx errors
              (server-error (err) (render-error err res)))))))

      ; cleanup form
      (format t "Server shut down!~%")
      (usocket:socket-close server-socket))))

(defun hello-world (req res)
  (format t "Hello-World: ~A -> ~A~%" (datetime-now) req)

  (cond
    ((equal (barghest.request:path req) "greeting")
     (render res (djula:render-template* (djula:compile-template* "greeting.html") nil :name (gethash "name" (barghest.request:args req)))))

    ((equal (barghest.request:path req) "")
     (render res (djula:render-template* (djula:compile-template* "index.html"))))

    (t (error 'client-error :err-code :404))))
