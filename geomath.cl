;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-08-27 22:50:30>

(in-package :virtualhelm)

(declaim (optimize speed (debug 0) (space 0) (safety 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

(defconstant +radius+
  6371229
  "Assumed radius of Earth in metres")

(defconstant +radius²+
  (* +radius+ +radius+)
  "Square of Earth radius")

(defconstant +pi/180+
  (/ pi 180))

(defconstant +deg-length+
  (/ (* 2 pi +radius+) 360)
  "Distance of 1° at the equator")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(defun round-to-digits (x d)
  (/ (fround x (expt 10 (- d))) (expt 10 d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpolation

(defun bilinear (w a w0 w1 a0 a1 v00 v01 v10 v11)
  ;; Bilinear interpolation at P=(w a) given values f(w0, a0) = v00 etc.
  (declare (double-float w a w0 w1 a0 a1 v00 v10 v01 v11))
  (assert (<= w0 w w1))
  (assert (<= a0 a a1))
  (let* ((dw
          (/ (- w w0) (- w1 w0)))
         (v0
          (+ v00 (* dw (- v01 v00))))
         (v1
          (+ v10 (* dw (- v11 v10))))
         (da
          (/ (- a a0) (- a1 a0)))
         (v
          (+ v0 (* da (- v1 v0)))))
    v))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric units

(defun deg (x)
  (* 360 (/ x (* 2 pi))))

(defun rad (x)
  (* (* 2 pi) (/ x 360)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;;; http://www.kompf.de/gps/distcalc.html
;;; http://www.kompf.de/trekka/distance.php?lat1=52.5164&lon1=13.3777&lat2=38.692668&lon2=-9.177944
;;; http://de.wikipedia.org/wiki/Gro%C3%9Fkreis

(defun course-distance (origin target)
  (let ((lat1 (rad (latlng-lat origin)))
        (lon1 (rad (latlng-lng origin)))
        (lat2 (rad (latlng-lat target)))
        (lon2 (rad (latlng-lng target))))
    (* +radius+
       (acos (+ (* (sin lat1) (sin lat2))
                (* (cos lat1) (cos lat2) (cos (- lon2 lon1))))))))

(defun course-angle (origin target)
  (let ((dist (course-distance origin target)))
    (when (eql dist 0d0)
      (error "Distance is zero between ~a and ~a" origin target))
    (let* ((e (/ dist +radius+))
           (lat1 (rad (latlng-lat origin)))
           (lat2 (rad (latlng-lat target)))
           (lon1 (rad (latlng-lng origin)))
           (lon2 (rad (latlng-lng target)))
           (cos-omega
            (/ (- (sin lat2) (* (sin lat1) (cos e)))
               (* (cos lat1) (sin e))))
           (omega
            (acos cos-omega))
           (delta
            (- lon2 lon1)))
      (deg
       (if (or (< delta 0)
               (> delta 180))
           (- omega)
           omega)))))
                     
(defun add-distance-exact! (pos distance alpha)
  ;; Exact calculation on the spherical Earth
  (let ((lat (latlng-lat pos))
        (lon (latlng-lng pos)))
    (declare (double-float lat lon distance alpha))
    (let* ((d (/ distance +radius+))
           (a (* alpha +pi/180+))
           (sin-a (sin a))
           (sin-d (sin d))
           (lat-r (* lat +pi/180+)) // phi
           (lon-r (* lon +pi/180+)) // lambda
           (lat-new-r (asin (+ (* (sin lat-r) (cos d))
                               (* (cos lat-r) sin-d (cos a)))))
           (lon-new-r (+ lon-r
                         (asin (/ (* sin-a sin-d)
                                  (cos lat-new-r))))))
      (setf (latlng-lat pos) (/ lat-new-r +pi/180+)
            (latlng-lng pos) (/ lon-new-r +pi/180+)))))

(defun add-distance-estimate! (pos distance alpha)
  ;; Approximation for short distances (<< 100km)
  (let ((lat (latlng-lat pos))
        (lon (latlng-lng pos)))
    (declare (double-float lat lon distance alpha))
    (let* ((d (/ distance +radius+))
           (a (* alpha +pi/180+))
           (lat-r (* lat +pi/180+))
           (lon-r (* lon +pi/180+))
           (d-lat-r (* d (cos a)))
           (d-lon-r (* d (/ (sin a) (cos (+ lat-r d-lat-r))))))
      (setf (latlng-lat pos) (/ (+ lat-r d-lat-r) +pi/180+)
            (latlng-lng pos) (/ (+ lon-r d-lon-r) +pi/180+)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting GRIB U/V values to DEG

(defun angle (u v)
  (declare (double-float u v))
  (+ 180d0 (* 180d0 (/ (atan u v) pi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Euclidian Norm

(defun enorm (x y)
  (declare (double-float x y))
  (sqrt (+ (* x x) (* y y))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
