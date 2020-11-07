;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2020-10-30 21:13:31>

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ice limit

(defparameter *constraints-dir*
  (merge-pathnames (make-pathname :directory '(:relative "constraints")) *source-root*))
  
(defun load-limits (filename)
  (let* ((path (merge-pathnames filename *constraints-dir*))
         (def (parse-json-file path))
         (south (joref (joref def "ice_limits") "south")))
    (map 'vector
         (lambda (p)
           (make-latlng :lat (coerce (joref p "lat") 'double-float)
                        :lng (coerce (joref p "lon") 'double-float)))
         south)))

(defun check-limits (p0 p1 limits)
  (loop
     :for k :from 1 :below (length limits)
     :never (segment-intersects p0 p1 (aref limits (1- k)) (aref limits k))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
