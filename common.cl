;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2023-03-17 19:56:44>


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
(defparameter *max-angle* 110)
(defparameter *max-iso-points* 1500)
(defparameter *max-route-hours* (* 24 12))

(defparameter *penalty-mode-vr* :dynamic)
(defparameter *penalty-mode-rs* :simple)

(defvar *api-key*)
(defvar *use-bitmap* nil)
(defvar *tracks* nil)
(defvar *resolutions* '("1p00" "0p25"))

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

(defparameter *twa-steps* nil
  "Set this to the desired TWA step width (eg. 5d0).
   NIL (default) mean use only points provided in polar file.
   Clear *combined-polars-ht* to force recomputation of cpolars.")

(defparameter +foil-speeds+ (map 'vector #'knots-to-m/s
                                #(0.0d0 11.0d0 16.0d0 35.0d0 40.0d0 70.0d0)) )
(defparameter +foil-angles+ #(0.0d0 70.0d0 80.0d0 160.0d0 170.0d0 180.0d0))
(defparameter +foil-matrix+ #2a((1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.04d0 1.04d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.04d0 1.04d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preventing eval injection

(defun read-arg (s &optional type)
  (let ((*read-eval* nil))
    (ignore-errors
     (let ((arg (read-from-string s)))
       (if type
           (coerce arg type)
           arg)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
