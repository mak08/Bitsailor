;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-09-02 22:16:39>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Todo: User setting
;;; Wind & Boat
;;; - select map [NOAA]|DWD
;;; - select polars [IMOCA60]|
;;; - sail change penalty [10%]
;;; - tack/gybe penalty [10%]
;;; Search parameters
;;; - land check [on]|off
;;; - long-step-threshold 3h|[6h]|12h|24h (?)
;;; - long-step-value 30min|[60min]
;;; - fan 25..85
;;; Display
;;; - display isochrones [on]|off
;;; - display tracks on|[off]
;;; Search fine-tuning
;;; - Minimum point distance [250]|500
;;; - Wake angle 20|[30]|40


(in-package :virtualhelm)

;;; time in seconds
(defconstant +5min+ (* 5 60))
(defconstant +10min+ (* 10 60))
(defconstant +20min+ (* 20 60))
(defconstant +30min+ (* 30 60))
(defconstant +60min+ (* 60 60))
(defconstant +3h+ (* 3 60 60))
(defconstant +4h+ (* 3 60 60))
(defconstant +6h+ (* 6 60 60))
(defconstant +12h+ (* 12 60 60))
(defconstant +24h+ (* 24 60 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A routing stores the start and destination of the route as well as
;;; routing parameters 

(defvar +FEHMARN+
  (make-latlng :lat 54.434403d0 :lng 11.361632d0))

(defvar +YSTAD+
  (make-latlng :lat 55.391123d0 :lng 13.792635d0))
        
(defstruct routing
  (start +fehmarn+)
  (dest +ystad+)
  twapath
  (fan 60)
  (angle-increment 1)
  (sectors 81)
  (points-per-sector 5)
  (stepmax +4h+)
  (stepsize +10min+))

(defstruct routeinfo tracks isochrones)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Isochrones are described by sets of routepoints.
;;;
;;; ### Think of a good sorting/data structure to support finding the most advanced point in a sector

(defstruct routepoint
  position
  heading
  twa
  speed
  sail
  is-land
  predecessor
  destination-angle
  destination-distance)

(defun get-route (routing)
  (let*
      ((forecast-bundle (or (get-forecast-bundle 'dwd-icon-bundle)
                            (get-forecast-bundle 'constant-wind-bundle)))
       (step-size (routing-stepsize routing))
       (angle-increment (routing-angle-increment routing))
       (start-pos (routing-start routing))
       (dest-pos (routing-dest routing))
       (isochrones nil))

    (log2:info "Routing from ~a to ~a (heading ~a)" start-pos dest-pos (course-angle start-pos dest-pos))

    (gm-to-grib! start-pos)
    (gm-to-grib! dest-pos)

    (do* ( ;; Iteration stops when destination was reached
          (reached nil)
          ;; Iteration stops when stepmax seconds have elapsed
          (stepsum 0
                   (+ stepsum step-size))
          (stepnum 0
                   (1+ stepnum))
          ;; The initial isochrone is just the start point, heading towards destination
          (isochrone (list (make-routepoint :position start-pos
                                            :heading  (round (course-angle start-pos dest-pos))
                                            :destination-distance (course-distance start-pos dest-pos)))
                     next-isochrone)
          ;; The next isochrone - in addition we need all hourly isochrones (tbd)
          (successors-per-point (1+ (truncate (* (routing-fan routing) 2) angle-increment)))
          (next-isochrone (make-array (* successors-per-point (length isochrone)))
                          (make-array (* successors-per-point (length isochrone))))
          (index 0 0)
          ;; Advance the simulation time after each iteration
          (step-time (now)
                     (adjust-timestamp step-time (:offset :sec step-size)))
          ;; Get wind data for simulation time
          (forecast (get-forecast forecast-bundle step-time)
                    (get-forecast forecast-bundle step-time)))
        
        ;; When the maximum number of iterations is reached, construct the best path
        ;; from the most advanced point's predecessor chain.
        ((or reached
             (>= stepsum (routing-stepmax routing)))
         (make-routeinfo :tracks (extract-tracks
                                  (construct-route isochrone))
                         :isochrones isochrones))
      (log2:info "Isochrone ~a at ~a, ~a points" stepnum step-time (length isochrone))
      ;; Iterate over each point in the current isochrone
      (map nil
           (lambda (routepoint)
             (let* ((dest-bearing (round (course-angle (routepoint-position routepoint) dest-pos)))
                    (left (- dest-bearing (routing-fan routing)))
                    (right (+ dest-bearing (routing-fan routing))))
               (loop
                  :for heading :from left :to right :by angle-increment
                  :for k :from 0
                  :do (multiple-value-bind (speed angle sail)
                          (heading-boatspeed forecast (routepoint-position routepoint) heading)
                        (let*
                            ((new-pos (add-distance-exact (routepoint-position routepoint)
                                                          (* speed step-size)
                                                          (coerce heading 'double-float)))
                             (dtf (course-distance new-pos dest-pos)))
                          (setf (aref next-isochrone index)
                                (make-routepoint :position new-pos
                                                 :heading heading
                                                 :twa angle
                                                 :speed speed
                                                 :sail sail
                                                 :is-land nil ;; compute this after wake pruning
                                                 :predecessor routepoint
                                                 :destination-angle (course-angle new-pos dest-pos)
                                                 :destination-distance dtf))
                          (incf index)
                          (when (< dtf 1000)
                            (unless reached
                              (log2:info "Reached destination at ~a" step-time)
                              (setf reached t))))))))
           isochrone)
      (setf next-isochrone (filter-isochrone next-isochrone stepnum))
      (multiple-value-bind (q r) (truncate stepsum 60)
        (when (zerop r)
          (push (map 'vector #'routepoint-position
                     (sort next-isochrone #'< :key #'routepoint-destination-angle))
                isochrones))))))
      
(defun filter-isochrone (isochrone stepnum)
  (setf isochrone (sort isochrone #'< :key #'routepoint-destination-distance))
  (loop
     :for p1 :across isochrone
     :do (when p1
           ;; It is not quite correct to skip covered points
           ;; because coveredness is computed w.r.t. the point's heading and thus is not transitive.
           (loop
              :for p2 :across isochrone
              :for k :from 0
              :do (when (and p2
                             (not (eq p1 p2))
                             (covers p1 p2 stepnum))
                    (setf (aref isochrone k) nil)))))
  (let ((result (loop
                   :for p :across isochrone
                   :when (and p
                              (not (is-land (latlng-lat (routepoint-position p))
                                            (latlng-lng (routepoint-position p)))))
                   :collect p)))
    (log2:info "Filter: in ~a, retaining ~a" (length isochrone) (length result))
    (make-array (length result) :initial-contents result)))

(defun covers (p1 p2 stepnum)
  (let ((dist (course-distance (routepoint-position p2)
                               (routepoint-position p1))))
    (or (case stepnum
          (0 (< dist 50))
          (1 (< dist 100))
          (2 (< dist 200))
          (3 (< dist 500))
          (otherwise
           (< dist 750)))
        (let ((angle (course-angle (routepoint-position p2)
                                   (routepoint-position p1)
                                   dist)))
          (< (abs (- angle (routepoint-heading p1))) 30)))))
           
(defun construct-route (isochrone)
  isochrone)

(defun extract-tracks (isochrone)
  (loop
     :for point :across isochrone
     :for k :from 0
     :collect (do ((p point (routepoint-predecessor p))
                   (v (list)))
                  ((null p)
                   v)
                (push (routepoint-position p) v))))

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
    (gm-to-grib! startpos)
    (gm-to-grib! destpos)
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
          (get-max-speed angle wind-speed)
        (values speed angle sail wind-speed)))))

(defun polars-angle (heading)
  (let ((angle (if (> heading 180)
                   (- heading 360)
                   heading)))
    (abs angle)))
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
