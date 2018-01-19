;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-12-29 21:31:52>

(in-package :virtualhelm)

(eval-when (:load-toplevel :execute)
  (defmacro check-equal (form result)
    `(progn
       (assert (equal ,form ,result))
       t)
    )
  )


(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng 1d0))
 44.99563645533813d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng 1d0))
 90.0d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 1d0))
 135.00436354466189d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 0d0))
 180.0d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng -1d0))
 -135.00436354466189d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng -1d0))
 -90.0d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng -1d0))
 -44.99563645533813d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng 0d0))
 0.0d0)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

