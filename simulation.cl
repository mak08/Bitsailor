;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-08-29 00:00:48>

(in-package :virtualhelm)

;;; time in seconds
(defconstant +5min+ (* 5 60))
(defconstant +10min+ (* 10 60))
(defconstant +30min+ (* 30 60))
(defconstant +60min+ (* 60 60))
(defconstant +3h+ (* 3 60 60))
(defconstant +12h+ (* 12 60 60))
(defconstant +24h+ (* 24 60 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A routing stores the start and destination of the route as well as
;;; routing parameters 

(defstruct routing
  start
  dest
  twapath
  route
  (fan 40)
  (sectors 81)
  (points-per-sector 5)
  (stepmax +12h+)
  (stepsize +5min+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Isochrones are described by sets of routepoints.
;;;
;;; ### Think of a good sorting/data structure to support finding the most advanced point in a sector

(defstruct routepoint
  position
  destination-angle
  destination-distance
  predecessor)

(defun get-route (routing)
  (let
      ((forecast-bundle (or (get-forecast-bundle 'dwd-icon-bundle)
                            (get-forecast-bundle 'constant-wind-bundle)))
       (step-size (routing-stepsize routing))
       (start-pos (routing-start routing))
       (dest-pos (routing-dest routing)))

    (dm-to-grib! start-pos)
    (dm-to-grib! dest-pos)

    (do* ((step 0 (1+ step))
          ;; The initial isochrone is just the start point.
          (isochrone (list (make-routepoint :position start-pos))
                     next-isochrone)
          ;; Advance the simulation time after each iteration
          (step-time (now) (adjust-timestamp steptime (:offset :sec step-size)))
          ;; Get wind data for simulation time
          (forecast (get-forecast forecast-bundle step-time)
                    (get-forecast forecast-bundle step-time)))
        
        ;; When the maximum number of iterations is reached, construct the best path
        ;; from the most advanced point's predecessor chain.
        ((>= step (routing-stepmax routing))
         (construct-route next-isochrone))

      ;; Iterate over each point in the current isochrone
      (dolist (point isochrone)
        (let ()
          (multiple-value-bind (speed heading)
              (twa-boatspeed forecast latlng twa)
            (add-distance-exact! latlng (* speed step-time) heading)))))))

(defun construct-route (isochrone)
  ())

  
  

(defun get-twa-path (routing
                     &key 
                       (total-time +12h+)
                       (step-num (truncate total-time (routing-stepsize routing))))
  (let* ((forecast-bundle (or (get-forecast-bundle 'dwd-icon-bundle)
                              (get-forecast-bundle 'constant-wind-bundle)))
         (start-time (now))
         (step-time (routing-stepsize routing))
         (startpos (routing-start routing))
         (destpos (routing-dest routing)))
    (when (< (latlng-lng startpos) 0) (incf (latlng-lng startpos) 360))
    (when (< (latlng-lng destpos) 0) (incf (latlng-lng destpos) 360))
    (let* ((heading (course-angle startpos destpos))
           (curpos (copy-latlng startpos))
           (wind-dir (get-wind-forecast (get-forecast forecast-bundle start-time) startpos))
           (twa (heading-twa wind-dir heading))
           (path nil))
      (dotimes (k step-num (reverse (push (copy-latlng curpos) path)))
        ;; Save current position
        (push (copy-latlng curpos) path)
        (adjust-timestamp! start-time (:offset :sec step-time))
        (let ((forecast (get-forecast forecast-bundle start-time)))
          (multiple-value-bind (speed heading)
              (twa-boatspeed forecast curpos twa)
            (add-distance-exact! curpos (* speed step-time) heading)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - HEADING values range from 0..360 (clockwise from north)
;;; - ANGLE  values range from -179..180 (starboard downwind to port downwind)  
;;; - DIRECTION values range from -179..180 

(defun twa-heading (wind-dir angle)
  "Compute HEADING resulting from TWA in WIND"
  (let ((heading
         (- wind-dir angle)))
    (if (> heading 360)
        (- heading 360)
        (if (< heading 0)
            (+ heading 360)
            heading))))

(defun heading-twa (wind-dir heading)
  "Compute TWA resulting from HEADING in WIND"
  (let ((angle (- wind-dir heading)))
    (if (<= angle -180)
        (+ angle 360)
        (if (> angle 180)
            (- angle 360)
            angle))))

(defun twa-boatspeed (forecast latlon angle)
  (multiple-value-bind (wind-dir wind-speed)
      (get-wind-forecast forecast latlon)
    (multiple-value-bind (speed sail)
        (get-max-speed (abs angle) wind-speed)
      (values speed
              (twa-heading wind-dir angle)
              sail
              wind-speed))))

(defun heading-boatspeed (forecast latlon heading)
  (multiple-value-bind (wind-dir wind-speed)
      (get-wind-forecast forecast latlon)
    (let ((angle (heading-twa wind-dir heading)))
      (multiple-value-bind (speed sail)
          (get-max-speed (abs angle) wind-speed)
        (values speed angle sail wind-speed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit conversion

(defun knots-to-m/s (knots)
  (* 463d0 (/ knots 900d0)))

(defun m/s-to-knots (m/s)
  (* 900d0 (/ m/s 463d0)))

(defun m/s-to-kM/H (m/s)
  (* m/s 3.6))

;;; Translate latitude coordinates of start and destination to [0..360)
(defun gm-to-grib! (latlng)
  (when (< (latlng-lng latlng) 0)
    (incf (latlng-lng latlng) 360)))  
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
