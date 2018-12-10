;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-12-10 21:22:40>

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
  44.99563645534485d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng 1d0))
 90.0d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 1d0))
 135.00436354465515d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng 0d0))
 180.0d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat -1d0 :lng -1d0))
 -135.00436354465518d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 0d0 :lng -1d0))
 -90.0d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng -1d0))
 -44.99563645534482d0)

(check-equal
 (course-angle (make-latlng :lat 0d0 :lng 0d0) (make-latlng :lat 1d0 :lng 0d0))
 0.0d0)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

