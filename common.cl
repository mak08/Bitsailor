;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2022-05-22 21:39:44>


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

(defconstant  +max-iso-points+ 1250)

(defvar *isochrones* nil)
(defvar *best-route*)

(defvar *api-key*)
(defvar *use-bitmap* t)
(defvar *manoeuvering-penalty* nil)
(defvar *tracks* nil)
(defvar *rs-max-hours* (* 24 15))
(defvar *vr-max-hours* (* 24 15))
(defvar *rs-gfs-resolution* "1p00")

(defun read-arg (s &optional type)
  (let ((*read-eval* nil))
    (ignore-errors
     (let ((arg (read-from-string s)))
       (if type
           (coerce arg type)
           arg)))))
       

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
