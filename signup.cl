;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-05-29 13:43:19>


(in-package :bitsailor)

(defvar *mailhost* "bitsailor.net")
(defvar *mailuser* "harbormaster")

(defparameter +email-template+ "
Hello,

you (or someone pretending to be you) have asked to register a user account at ~a.
If this wasn't you, please ignore this email and your address will be removed from our records.

To activate this account, please click the following link within the next 24 hours:

https://~a/activate-account/~a

For questions,tips and suggestions, please join us on Discord:
https://discord.gg/ysnbv4Ke


Best Regards
~a

--

")

;;; Invoke the local mail command as MAILUSER and return the activation URL; mail-command errors
;;; are suppressed.
(defun send-email (email activation-secret boat &key (mailuser *mailuser*) (hostname *mailhost*))
  (declare (ignore boat))
  (let* ((mailcmd "mail")
         (recipient email)
         (subject "Activation link")  
         (in (make-string-input-stream (format nil +email-template+ hostname hostname activation-secret mailuser)))
         (out (make-string-output-stream))
         (activation-link (format nil "https://~a/activate-account/~a" hostname activation-secret))
         (command (format nil "su ~a -c '~a -s \"~a\" -S \"from=~a@~a\" ~a'" mailuser mailcmd subject mailuser hostname recipient)))
    (log2:info "Command: ~a~%" command)
    (ignore-errors
     (uiop:run-program command :input in :output out)
     (get-output-stream-string out))
    activation-link))

;;; Reserve a boat name in USER_PROV, returning (NIL :BOATNAME-EXISTS) for another email's
;;; reservation or (T :OK).
(defun register-signup (email activation-secret boatname pwhash)
  ;; Check if the boat name can be reserved.
  ;; Return NIL and a reason keyword on conflict; otherwise upsert USER_PROV.
  ;; Activation later promotes this provisional record to USER.
  (let* ((user
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
       (add-user-provisional email pwhash boatname "active" activation-secret)
       (values t :ok)))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
