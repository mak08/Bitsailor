;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2023
;;; Last Modified <michael 2023-06-03 17:13:55>

(defpackage :curl
  (:use :cl)
  (:export "HTTP"
           "HTTP-RESPONSE-STATUS"
           "HTTP-RESPONSE-HEADERS"
           "HTTP-RESPONSE-BODY"
           "HTTP-HEADER-NAME"
           "HTTP-HEADER-VALUE"
           "HTTP-STATUS-PROTOCOL"
           "HTTP-STATUS-CODE"
           "HTTP-STATUS-TEXT"))

(in-package :curl)

(defstruct http-response status headers body)
(defstruct http-status protocol code text)
(defstruct http-header name value)

(defun http (url &key (headers ()) (method :get) (body nil))
  (ecase method ((:get :head)))
  (let* ((head-flag (if (eq method :head) "-I" ""))
         (rbody (cond
                  ((and body (eq method :head))
                   (log2:warning "HEAD request, suppressing body")
                   nil)
                  (t
                   body)))
         (ftp-command
           (format () "curl --max-time 3 -i ~a ~:{ -H '~a: ~a'~} ~:[~;--data-raw ~:*'~a'~] \"~a\"" head-flag headers rbody url))
         (out-stream (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t))
         (err-stream (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t)))
    (log2:trace "~a" ftp-command)
    (with-output-to-string (out out-stream)
      (with-output-to-string (err err-stream)
        (multiple-value-bind (out-status err-status program-status)
            (uiop:run-program ftp-command :ignore-error-status t :output out :error-output err)
          (declare (ignore out-status err-status))
          (case program-status
            (0
             (parse-http-response out-stream :fetch-body (not (eq method :head))))
            (3
             (error "curl: error 3 (URL malformed)"))
            (6
             (error "curl: error 6 (Couldn't resolve host)"))
            (7
             (error "curl: error 7 (Failed to connect to host)"))
            (t
             (error "curl: error ~a" program-status))))))))

(defun parse-http-response (s &key (fetch-body t))
  (with-input-from-string (f s)
    (let*
        ((status-line
          (parse-status-line (read-line f nil nil)))
         (headers
          (loop
             :for line = (read-line f nil nil)
             :while  (> (length line) 1)
             :collect (parse-http-header line)))
         (body
           (cond
             (fetch-body
              (let* ((content-length-header
                       (find "Content-Length" headers :test #'string-equal :key #'http-header-name))
                     (content-length
                       (if content-length-header
                           (parse-integer
                            (http-header-value content-length-header))
                           (error "Can't read without content-length")))
                     (buffer
                       (make-array content-length :element-type 'character)))
                (read-sequence buffer f)
                buffer))
             (t
              #()))))
      (make-http-response :status status-line
                          :headers headers
                          :body body))))

(defun parse-status-line (s)
  (log2:trace "~a" s) 
  (let* ((p1 (position #\Space s))
         (p2 (position #\Space s :start (1+ p1)))) 
    (make-http-status :protocol (subseq s 0 p1)
                      :code (parse-integer (subseq s (1+ p1) p2))
                      :text (string-trim " "
                                         (subseq s (1+ p2))))))

(defun parse-http-header (s)
  (log2:trace "~a" s) 
  (let ((p (position #\: s)))
    (make-http-header :name (subseq s 0 p)
                      :value (string-trim " " (subseq s (1+ p))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
