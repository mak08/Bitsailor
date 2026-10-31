;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2023-02-21 21:51:37>

(in-package :bitsailor)


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

;;; Clear all race-specific constraint registrations.
(defun remove-constraints ()
  (clrhash +race-constraints-ht+))

;;; Prepend CONSTRAINT to the list registered for RACE-ID.
(defun add-constraint (race-id constraint)
  (push constraint (gethash race-id +race-constraints-ht+)))

;;; Return the constraints registered for RACE-ID, or NIL when none exist.
(defun get-constraints (race-id)
  (gethash race-id +race-constraints-ht+))

;;; Dispatch a segment/endpoint constraint check; the default method signals that the constraint
;;; is unimplemented.
(defgeneric meets (constraint point predecessor)
  (:method (constraint point predecessor)
    (error "Constraint ~a has no implementation" constraint)))

;;; Require every supplied constraint to accept POINT and PREDECESSOR; an empty list succeeds.
(defun meets-all (constraints point predecessor)
  (every (lambda (constraint)
           (meets constraint point predecessor))
         constraints))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ice limit

(defparameter *constraints-dir*
  (merge-pathnames (make-pathname :directory '(:relative "constraints")) *source-root*))
  
;;; Read the south ice-limit coordinates from a JSON file relative to the constraints directory.
(defun load-limits (filename)
  (let* ((path (merge-pathnames filename *constraints-dir*))
         (def (parse-json-file path))
         (south (joref (joref def "ice_limits") "south")))
    (map 'vector
         (lambda (p)
           (make-latlng :lat (coerce (joref p "lat") 'double-float)
                        :lng (coerce (joref p "lon") 'double-float)))
         south)))

(declaim (inline check-limits))
;;; Return true when segment P0-P1 intersects none of the consecutive segments in LIMITS.
(defun check-limits (p0 p1 limits)
  (log2:trace "Limits ~a pts" (length limits))
  (loop
     :for k :from 1 :below (length limits)
     :never (segment-intersects p0 p1 (aref limits (1- k)) (aref limits k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constraint types - Limits

(defclass limit-south (latitude-constraint)
  ())
;;; Accept points strictly north of the latitude limit; ignore the predecessor.
(defmethod meets ((constraint limit-south) (point vector) predecessor)
  (declare (ignore predecessor))
  (> (latlng-lat point) (latitude constraint)))

(defclass limit-north (latitude-constraint)
  ())
;;; Accept points strictly south of the latitude limit; ignore the predecessor.
(defmethod meets ((constraint limit-north) (point vector) predecessor)
  (declare (ignore predecessor))
  (< (latlng-lat point) (latitude constraint)))


(defclass limit-east (longitude-constraint)
  ())
;;; Legacy longitude crossing predicate specialized on the base class. The later method with the
;;; same specializers replaces the earlier one; these do not distinguish limit-east from limit-
;;; west.
(defmethod meets ((constraint longitude-constraint) (point vector) (predecessor vector))
  (not (longitude-between (latlng-lng predecessor) (latlng-lng point) (longitude constraint))))

(defclass limit-west (longitude-constraint)
  ())
;;; Legacy longitude crossing predicate specialized on the base class. The later method with the
;;; same specializers replaces the earlier one; these do not distinguish limit-east from limit-
;;; west.
(defmethod meets ((constraint longitude-constraint) (point vector) (predecessor vector))
  (not (longitude-between (latlng-lng point) (latlng-lng predecessor) (longitude constraint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eastbound gates

(defclass eastbound-south-gate (point-constraint)
  ())
;;; Require the point south of the latitude when the segment crosses the gate longitude eastbound.
(defmethod meets ((constraint eastbound-south-gate) (point vector) (predecessor vector))
  (or (< (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng predecessor) (latlng-lng point) (longitude constraint)))))

(defclass eastbound-north-gate (point-constraint)
  ())
;;; Require the point north of the latitude when the segment crosses the gate longitude eastbound.
(defmethod meets ((constraint eastbound-north-gate) (point vector) (predecessor vector))
  (or (> (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng predecessor) (latlng-lng point) (longitude constraint)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Westbound gates

(defclass westbound-south-gate (point-constraint)
  ())
;;; Require the point south of the latitude when the segment crosses the gate longitude westbound.
(defmethod meets ((constraint westbound-south-gate) (point vector) (predecessor vector))
  (or (< (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng point) (latlng-lng predecessor) (longitude constraint)))))

(defclass westbound-north-gate (point-constraint)
  ())
;;; Require the point north of the latitude when the segment crosses the gate longitude westbound.
(defmethod meets ((constraint westbound-north-gate) (point vector) (predecessor vector))
  (or (> (latlng-lat point) (latitude constraint))
      (not (longitude-between (latlng-lng point) (latlng-lng predecessor) (longitude constraint)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
