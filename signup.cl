;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-05-16 23:36:43>


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
  (declare (ignore boat))
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

(defun register-signup (email activation-secret boatname password)
  ;; Check if the boat name can be reserved.
  ;; If not, return NIL and a message describing the reason.
  ;; If yes, reserve boatname by creating an inactive entry in USER.
  ;; Return t.
  (let ((user
          (get-user-by-boatname boatname))
        (provisional
          (get-user-prov-by-boatname boatname)))
    (cond
      ((or (and user
                (not (string-equal (email user) email)))
           (and provisional
                (not (string-equal (email provisional) email))))
       (values nil :boatname-exists))
      (t
       ;; Boatname not taken or belongs to current email
       (add-user-provisional email password boatname "active" activation-secret)
       (values t :ok)))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
