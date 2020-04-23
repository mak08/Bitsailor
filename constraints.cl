;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2020-04-23 18:10:52>

(in-package :virtualhelm)


(defclass constraint () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The basic constraint types just provide slot definitions for the actual
;;; constraint types. There is no inherent logic.

(defclass latitude-constraint (constraint)
  ((latitude :reader latitude :initarg :latitude)))
(defclass longitude-constraint (constraint)
  ((longitude :reader longitude :initarg :longitude)))
(defclass point-constraint (latitude-constraint longitude-constraint)
  ())

(defparameter +race-constraints-ht+ (make-hash-table :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API functions

(defun remove-constraints ()
  (clrhash +race-constraints-ht+))

(defun add-constraint (race-id constraint)
  (push constraint (gethash race-id +race-constraints-ht+)))

(defun get-constraints (race-id)
  (gethash race-id +race-constraints-ht+))

(defgeneric meets (constraint point predecessor)
  (:method (constraint point predecessor)
    (error "Constraint ~a has no implementation" constraint)))

(defun meets-all (constraints point predecessor)
  (every (lambda (constraint)
           (meets constraint point predecessor))
         constraints))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constraint types - Limits

(defclass limit-south (latitude-constraint)
  ())
(defmethod meets ((constraint limit-south) (point vector) predecessor)
  (declare (ignore predecessor))
  (> (latlng-lat point) (latitude constraint)))

(defclass limit-north (latitude-constraint)
  ())
(defmethod meets ((constraint limit-north) (point vector) predecessor)
  (declare (ignore predecessor))
  (< (latlng-lat point) (latitude constraint)))


(defclass limit-east (longitude-constraint)
  ())
(defmethod meets ((constraint longitude-constraint) (point vector) (predecessor vector))
  (not (longitude-between (latlng-lng predecessor) (latlng-lng point) (longitude constraint))))

(defclass limit-west (longitude-constraint)
  ())
(defmethod meets ((constraint longitude-constraint) (point vector) (predecessor vector))
  (not (longitude-between (latlng-lng point) (latlng-lng predecessor) (longitude constraint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eastbound gates

(defclass eastbound-south-gate (point-constraint)
  ())
(defmethod meets ((constraint eastbound-south-gate) (point vector) (predecessor vector))
  (or (< (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng predecessor) (latlng-lng point) (longitude constraint)))))

(defclass eastbound-north-gate (point-constraint)
  ())
(defmethod meets ((constraint eastbound-north-gate) (point vector) (predecessor vector))
  (or (> (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng predecessor) (latlng-lng point) (longitude constraint)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Westbound gates

(defclass westbound-south-gate (point-constraint)
  ())
(defmethod meets ((constraint westbound-south-gate) (point vector) (predecessor vector))
  (or (< (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng point) (latlng-lng predecessor) (longitude constraint)))))

(defclass westbound-north-gate (point-constraint)
  ())
(defmethod meets ((constraint westbound-north-gate) (point vector) (predecessor vector))
  (or (> (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng point) (latlng-lng predecessor) (longitude constraint)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;F
