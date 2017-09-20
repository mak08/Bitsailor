;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-09-20 23:38:15>

(in-package :virtualhelm)

(declaim (optimize (speed 3) (debug 1)  (space 0) (safety 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

(defconstant +radius+
  6371229d0
  "Assumed radius of Earth in metres")

(defconstant +radius²+
  (* +radius+ +radius+)
  "Square of Earth radius")

(defconstant +pi/180+
  (/ pi 180))

(defconstant +deg-length+
  (/ (* 2 pi +radius+) 360)
  "Distance of 1° at the equator")

(defconstant +eps+ 1d-6)

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
          (if (= w0 w1) 0d0 (/ (- w w0) (- w1 w0))))
         (v0
          (+ v00 (* dw (- v01 v00))))
         (v1
          (+ v10 (* dw (- v11 v10))))
         (da
          (if (= a0 a1) 0d0 (/ (- a a0) (- a1 a0))))
         (v
          (+ v0 (* da (- v1 v0)))))
    (declare (double-float dw v0 v1 da v))
    v))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric units

(defun deg (x)
  (* 360 (/ x (* 2 pi))))

(defun rad (x)
  (declare (double-float x))
  (* (* 2d0 pi) (/ x 360d0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;;; http://www.kompf.de/gps/distcalc.html
;;; http://www.kompf.de/trekka/distance.php?lat1=52.5164&lon1=13.3777&lat2=38.692668&lon2=-9.177944
;;; http://de.wikipedia.org/wiki/Gro%C3%9Fkreis

(defun course-distance (origin target)
  (let* ((lat1 (latlng-latr origin))
         (lon1 (latlng-lngr origin))
         (lat2 (latlng-latr target))
         (lon2 (latlng-lngr target))
         (cis-lat1 (cis lat1))
         (cos-lat1 (realpart cis-lat1))
         (sin-lat1 (imagpart cis-lat1))
         (cis-lat2 (cis lat2))
         (cos-lat2 (realpart cis-lat2))
         (sin-lat2 (imagpart cis-lat2)))
    (declare (double-float lat1 lon1 lat2 lon2 cos-lat1 sin-lat1 cos-lat2 sin-lat2))
    (* +radius+
       (acos (+ (* sin-lat1 sin-lat2)
                (* cos-lat1 cos-lat2 (cos (- lon2 lon1))))))))

(defun course-angle (origin target &optional (dist nil))
  (let* ((lat1 (latlng-latr origin))
         (cis-lat1 (cis lat1))
         (cos-lat1 (realpart cis-lat1))
         (sin-lat1 (imagpart cis-lat1))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target)))
    (declare (double-float lat1 cos-lat1 sin-lat1 lon1 lat2 lon2))
    (cond
      ((and
        (eql lat1 lat2)
        (eql lon1 lon2))
       (error "Distance is zero between ~a and ~a" origin target))
      ((< (abs (- lat1 lat2)) +eps+)
       (if (< lon1 lon2)
           90d0
           -90d0))
      ((< (abs (- lon1 lon2)) +eps+)
       (if (< lat1 lat2)
           0d0
           180d0))
      (t
       (let ((dist (or dist (course-distance origin target))))
         (when (eql dist 0d0)
           (error "Distance is zero between ~a and ~a" origin target))
         (when (complexp dist)
           (error "Invalid distance |~a ~a|" origin target))
         (let* ((e (/ dist +radius+))
                (cos-omega
                 (/ (- (sin lat2) (* sin-lat1 (cos e)))
                    (* cos-lat1 (sin e))))
                (omega
                 (acos cos-omega))
                (delta
                 (- lon2 lon1)))
           (declare (double-float cos-omega omega delta))
           (when (complexp omega)
             (error "Invalid cos-omega ~a for origin=~a target=~a" cos-omega origin target))
           (deg
            (if (or (< delta 0d0)
                    (> delta 180d0))
                (- omega)
                omega))))))))

(defun gc-angle (origin target)
  ;; Angle between origin and target on the GC defined by these points (symmetric)
  (let* ((lat1 (latlng-latr origin))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target))
         (d (- lon2 lon1)))
    (deg
     (acos
      (+ (* (sin lat1) (sin lat2))
         (* (cos lat1) (cos lat2) (cos d)))))))
                     
(defun add-distance-exact (pos distance alpha)
  ;; Exact calculation on the spherical Earth
  (let ((lat (latlng-lat pos))
        (lon (latlng-lng pos)))
    (declare (double-float lat lon distance alpha))
    (let* ((d (/ distance +radius+))
           (cis-d (cis d))
           (cos-d (realpart cis-d))
           (sin-d (imagpart cis-d))
           (a (* alpha +pi/180+))
           (sin-a (sin a))
           (lat-r (* lat +pi/180+))
           (cis-lat-r (cis lat-r))
           (cos-lat-r (realpart cis-lat-r))
           (sin-lat-r (imagpart cis-lat-r))
           (lon-r (* lon +pi/180+))
           (lat-new-r (asin (+ (* sin-lat-r cos-d)
                               (* cos-lat-r sin-d (cos a)))))
           (lon-new-r (+ lon-r
                         (asin (/ (* sin-a sin-d)
                                  (cos lat-new-r))))))
      (declare (double-float cos-d sin-d cos-lat-r sin-lat-r))
      (make-latlng :lat (/ lat-new-r +pi/180+)
                   :lng (/ lon-new-r +pi/180+)))))

;; Can't SETF LATLNG-LAT/LNG!
#+()(defun add-distance-exact! (pos distance alpha)
  ;; Exact calculation on the spherical Earth
  (let ((lat (latlng-lat pos))
        (lon (latlng-lng pos)))
    (declare (double-float lat lon distance alpha))
    (let* ((d (/ distance +radius+))
           (a (* alpha +pi/180+))
           (sin-a (sin a))
           (sin-d (sin d))
           (lat-r (* lat +pi/180+))
           (lon-r (* lon +pi/180+))
           (lat-new-r (asin (+ (* (sin lat-r) (cos d))
                               (* (cos lat-r) sin-d (cos a)))))
           (lon-new-r (+ lon-r
                         (asin (/ (* sin-a sin-d)
                                  (cos lat-new-r))))))
      (setf (latlng-lat pos) (/ lat-new-r +pi/180+)
            (latlng-lng pos) (/ lon-new-r +pi/180+)))))

(defun add-distance-estimate (pos distance alpha)
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
      (make-latlng :lat (/ (+ lat-r d-lat-r) +pi/180+)
                   :lng (/ (+ lon-r d-lon-r) +pi/180+)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting GRIB U/V values to DEG

(defun angle (u v)
  (declare (double-float u v))
  (let ((angle
         (+ 180d0 (* 180d0 (/ (atan u v) pi)))))
    angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Euclidian Norm

(defun enorm (x y)
  (declare (double-float x y))
  (sqrt (+ (* x x) (* y y))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
