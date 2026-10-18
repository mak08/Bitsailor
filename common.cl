;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-02-05 23:33:00>


(in-package :bitsailor)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameters and configuration

(defparameter *disable-nmea*  nil)
(defparameter *max-angle* 120)
(defparameter *max-iso-points* 1500)
(defparameter *max-route-hours* (* 24 12))

(defvar *use-bitmap* nil)
(defvar *tracks* nil)
(defvar *web-root-directory*
  (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                    '("web"))))

(defvar *polars-dir*
  (merge-pathnames (make-pathname :directory '(:relative "polars") :type "json")
                   *source-root*)
  "A string designating the directory containing polar files")

(defvar *races-dir*
  (merge-pathnames (make-pathname :directory '(:relative "races") :type "json")
                   *source-root*)
  "A string designating the directory containing race definitions")

(defparameter *twa-steps* 5d0
  "Set this to the desired TWA step width (eg. 5d0).
   NIL (default) mean use only points provided in polar file.
   Clear *combined-polars-ht* to force recomputation of cpolars.")


;;; Loading race lists
(defvar *racelist-timer* nil)

;;; Statistics
(defvar *statistics-timer* nil)
(defvar *last-request* (list))
(defvar +last-request-lock+ (bordeaux-threads:make-lock "LAST-REQUEST-LOCK"))
(defvar *last-routestats* nil)
(defvar +last-routestats-lock+ (bordeaux-threads:make-lock "LAST-ROUTESTATS"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preventing eval injection

(defun read-arg (s &optional type (package *package*))
  (let ((*read-eval* nil))
    (ignore-errors
     (let* ((*package* (find-package package))
            (arg (read-from-string s)))
       (if type
           (coerce arg type)
           arg)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
