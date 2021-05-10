;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-05-10 22:45:38>


(in-package :virtualhelm)

;;; mail -s "This is the subject line" -S "from=no-reply@sailsphere.com" michael.kappert@gmx.net

(defun send-comfirmation-mail ()
  (let ()
    (uiop:run-program "mail -s \"This is the subject line\" -S \"from=no-reply@sailsphere.com\" michael.kappert@gmx.net" :input t)))

(defun register-signup (email activation-secret boat password)
  ;; Reserve boatname by creating an inactive entry in USER.
  (let ((user
          (ignore-errors
           (add-user email password boat "inactive" activation-secret))))
    ;; Send an email with the activation link
    ( user
      
      )))

(defun send-email (email activation-secret boat)
  )

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
