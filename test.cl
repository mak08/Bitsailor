;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-12-10 23:57:31>

(in-package :virtualhelm)

(eval-when (:load-toplevel :execute)
  (defmacro check-delta-equal (form result)
    `(progn
       (assert (equal (ftruncate ,form 0.00001) (ftruncate ,result 0.00001)))
       t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng 1d0))
  44.99563d0)

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng 1d0))
 90.0d0)

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 1d0))
 135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 0d0))
 180.0d0)

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng -1d0))
 -135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng -1d0))
 -90.0d0)

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng -1d0))
 -44.9956364d0)

(check-delta-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng 0d0))
 0.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng 1d0))
  44.9956365d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng 1d0))
 90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 1d0))
 135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 0d0))
 180.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng -1d0))
 -135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng -1d0))
 -90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng -1d0))
 -44.995636455d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng 0d0))
 0.0d0)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

