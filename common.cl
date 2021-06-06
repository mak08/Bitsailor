;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-06-06 01:52:29>


(in-package "VIRTUALHELM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common in all modules

(defparameter *source-root*
  (make-pathname :directory (pathname-directory #.*compile-file-truename*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific  to this module

;;; time in seconds
(defconstant +10min+ (* 10 60))
(defconstant +12h+ (* 12 60 60))
(defconstant +24h+ (* 24 60 60))

(defconstant  +max-iso-points+ 1200)

(defvar *isochrones* nil)
(defvar *best-route*)

(defvar *use-bitmap* t)
(defvar *manoeuvering-penalty* nil)
(defvar *tracks* nil)

(defun read-arg (s)
  (let ((*read-eval* nil))
    (ignore-errors (read-from-string s))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
