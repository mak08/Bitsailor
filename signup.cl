;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-05-10 21:22:51>


(in-package :virtualhelm)

;;; mail -s "This is the subject line" -S "from=no-reply@sailsphere.com" michael.kappert@gmx.net

(defun send-comfirmation-mail ()
  (let ()
    (uiop:run-program "mail -s \"This is the subject line\" -S \"from=no-reply@sailsphere.com\" michael.kappert@gmx.net" :input t)))

(defun register-signup (email link-secret boat password)
  )

(defun send-email (email link-secret boat)
  )

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
