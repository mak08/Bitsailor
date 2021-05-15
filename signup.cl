;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-05-13 01:20:20>


(in-package :virtualhelm)

(defparameter +email-template+ "
Hello,

you (or someone pretending to be you) have asked to register a user account at sailsphere.com.
If this wasn't you, please ignore this email and your address will be removed from our records.

To activate this account, please click the following link within the next 24 hours:

https://sailsphere.com/activate-account/~a

Best Regards

--

")

(defun send-email (email activation-secret boat)
  (let* ((mailcmd "mail")
         (recipient email)
         (subject "Activation link")  
         (in (make-string-input-stream (format nil +email-template+ activation-secret)))
         (out (make-string-output-stream))
         (activation-link (format nil "http:aguas-13:8080/activate-account/~a" activation-secret))
         (command (format nil "su rs -c '~a -s \"~a\" -S \"from=rs@sailsphere.com\" ~a'" mailcmd subject recipient)))
    (log2:info "Command: ~a~%" command)
    (ignore-errors
     (uiop:run-program command :input in :output out)
     (get-output-stream-string out))
    activation-link))

(defun register-signup (email activation-secret boat password)
  ;; Reserve boatname by creating an inactive entry in USER.
  (let ((user
          (ignore-errors
           (add-user email password boat "inactive" activation-secret))))
    user))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
