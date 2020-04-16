(defpackage barghest.http
  (:use :cl)
  (:export #:code
           #:description
           #:err-code
           #:err-description
           #:make-status-code))
(in-package :barghest.http)

(defparameter codes
  '(; Informational response
    :100 "Continue"
    :101 "Switching Protocols"
    :102 "Processing"
    :103 "Early Hints"

    ; Success
    :200 "OK"
    :201 "Created"
    :202 "Accepted"
    :203 "Non-Authoritive Information"
    :204 "No Content"
    :205 "Reset Content"
    :206 "Partial Content"
    :207 "Multi-Status"
    :208 "Already Reported"
    :226 "IM Used"

    ; Redirect
    :300 "Multiple Choices"
    :301 "Moved Permanently"
    :302 "Found"
    :303 "See Other"
    :304 "Not Modified"
    :305 "Use Proxy"
    :306 "Switch Proxy"
    :307 "Temporary Redirect"
    :308 "Permanent Redirect"

    ; Client Error
    :400 "Bad Request"
    :401 "Unauthorized"
    :402 "Payment Required"
    :403 "Forbidden"
    :404 "Not Found"
    :405 "Method Not Allowed"
    :406 "Not Acceptable"
    :407 "Proxy Authentication Required"
    :408 "Require Timeout"
    :409 "Conflict"
    :410 "Gone"
    :411 "Length Required"
    :412 "Precondition Failed"
    :413 "Payload Too Large"
    :414 "URI Too Long"
    :415 "Unsupported Media Type"
    :416 "Range Not Satisfiable"
    :417 "Expectation Failed"
    :418 "I'm a teapot"
    :421 "Misdirected Request"
    :422 "Unprocessable Entity"
    :423 "Locked"
    :424 "Failed Dependency"
    :425 "Too Early"
    :426 "Upgrade Required"
    :428 "Precondition Required"
    :429 "Too Many Requests"
    :431 "Request Header Fields Too Large"
    :451 "Unavailable For Legal Reasons"

    ; Sever Error
    :500 "Internal Server Error"
    :501 "Not Implemented"
    :502 "Bad Gateway"
    :503 "Service Unavailable"
    :504 "Gateway Timeout"
    :505 "HTTP Version Not Supported"
    :506 "Variant Also Negotiates"
    :507 "Insufficient Storage"
    :508 "Loop Detected"
    :510 "Not Extended"
    :511 "Network Authentication Required"))

(defclass status-code ()
  ((code        :initarg :code        :initform "" :reader code)
   (description :initarg :description :initform "" :reader description)))

(defgeneric code (obj)
  (:documentation "Gets the code of the status code object"))

(defgeneric description (obj)
  (:documentation "Gets the code of the status code object"))

(defmethod print-object ((object status-code) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~A" (code object) (description object))))

(defun make-status-code (code)
  (let ((status (getf codes code)))
    (make-instance 'status-code :code code :description status)))

(define-condition http-error (error)
  ((err-code :initarg :err-code :initform (error "Must provide a code") :reader err-code))
  (:report (lambda (condition stream)
             (let ((a-code (make-status-code (err-code condition))))
                (format stream "~A: ~A" (code a-code) (description a-code))))))

(defgeneric err-code (obj)
  (:documentation "Gets the code of the http-error object"))

(define-condition client-error (http-error)
  ((code :initarg :err-code :initform :400 :reader err-code)))

(define-condition server-error (http-error)
  ((code :initarg :err-code :initform :500 :reader err-code)))
