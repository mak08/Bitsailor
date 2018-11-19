;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2018-11-14 23:57:49>

(in-package :virtualhelm)

(declaim (optimize (speed 3) (debug 1)  (space 0) (safety 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

(defconstant +radius+
  ;; 6371229d0
  6218884d0
  "Assumed radius of Earth in metres")

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

(defun fraction-index (value steps)
  (loop
     :for step :across steps
     :for index :from 0
     :while (and (< index (length steps))
                 (<= step value))
     :finally (return (values (1- index)
                              (/ (- value (aref steps (1- index)))
                                 (- step (aref steps (1- index))))))))

(defun bilinear-unit (x y f00 f01 f10 f11)
  (+ (* f00 (- 1 x) (- 1 y))
     (* f01 x (- 1 y))
     (* f10 (- 1 x) y)
     (* f11 x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric units

(declaim (inline deg))
(defun deg (x)
  (declare (double-float x))
  (* 360 (/ x (* 2 pi))))
(declaim (notinline deg))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;;; http://www.kompf.de/gps/distcalc.html
;;; http://www.kompf.de/trekka/distance.php?lat1=52.5164&lon1=13.3777&lat2=38.692668&lon2=-9.177944
;;; http://de.wikipedia.org/wiki/Gro%C3%9Fkreis

(defun course-distance (origin target)
  (declare (ftype (function (t) double-float) latlng-latr latlng-lngr))
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

(defun course-angle (origin target &optional (dist -1d0))
  (let* ((lat1 (latlng-latr origin))
         (cis-lat1 (cis lat1))
         (cos-lat1 (realpart cis-lat1))
         (sin-lat1 (imagpart cis-lat1))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target)))
    (declare (double-float dist lat1 cos-lat1 sin-lat1 lon1 lat2 lon2))
    (cond
      ((and
        (eql lat1 lat2)
        (eql lon1 lon2))
       (error "Distance is zero between ~a and ~a" origin target))
      (t
       (when (eql dist -1d0)
         (setf dist (course-distance origin target)))
       (when (eql dist 0d0)
         (error "Distance is zero between ~a and ~a" origin target))
       (when (complexp dist)
         (error "Invalid distance |~a ~a|" origin target))
       (let* ((e (/ dist +radius+))
              (cos-omega
               (/ (- (sin lat2) (* sin-lat1 (cos e)))
                  (* cos-lat1 (sin e))))
              (omega
               (let ((omega%
                      (acos cos-omega)))
                 (if (complexp omega%)
                     (realpart omega%)
                     omega%)))
              (delta
               (- lon2 lon1)))
         (declare (double-float cos-omega omega delta))
         (deg
          (if (or (< delta 0d0)
                  (> delta 180d0))
              (- omega)
              omega)))))))

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


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
