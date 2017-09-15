;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-09-15 23:13:21>

;; -- stats: min/max points per isochrone
;; -- delete is-land after filtering isochrone
;; -- marks
;; -- atan/acos may return #C() => see CLTL
;; -- use CIS ?

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



(in-package :virtualhelm)

;;; time in seconds
(defconstant +5min+ (* 5 60))
(defconstant +10min+ (* 10 60))
(defconstant +20min+ (* 20 60))
(defconstant +30min+ (* 30 60))
(defconstant +60min+ (* 60 60))
(defconstant +3h+ (* 3 60 60))
(defconstant +4h+ (* 4 60 60))
(defconstant +6h+ (* 6 60 60))
(defconstant +12h+ (* 12 60 60))
(defconstant +24h+ (* 24 60 60))
(defconstant +48h+ (* 48 60 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A routing stores the start and destination of the route as well as
;;; routing parameters 

(defvar +FEHMARN+
  (make-latlng :lat 54.434403d0 :lng 11.361632d0))

(defvar +YSTAD+
  (make-latlng :lat 55.391123d0 :lng 13.792635d0))
        
(defstruct routing
  (forecast-bundle 'dwd-icon-bundle)
  (polars "VO65")
  (start +fehmarn+)
  (dest +ystad+)
  (fan 75)
  (angle-increment 1)
  (max-points-per-isochrone 100)
  (stepmax +24h+)
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
  origin-angle
  sort-angle%
  destination-angle
  destination-distance)

(defun get-route (routing)
  (let ((start-pos (routing-start routing))
        (dest-pos (routing-dest routing)))
    ;; Can't modify a latlng!
    ;; (gm-to-grib! start-pos)
    ;; (gm-to-grib! dest-pos)
    (if (>= (- (latlng-lng dest-pos) (latlng-lng start-pos)) 180)
        (setf dest-pos
              (make-latlng :lat (latlng-lat dest-pos)
                           :lng (- (latlng-lng dest-pos) 360))))

    (let*
        ((forecast-bundle (or (get-forecast-bundle (routing-forecast-bundle routing))
                              (get-forecast-bundle 'constant-wind-bundle)))
         (polars-name (routing-polars routing))
         (step-size (routing-stepsize routing))
         (angle-increment (routing-angle-increment routing))
         (max-points (routing-max-points-per-isochrone routing))
         (dest-heading (round (course-angle start-pos dest-pos)))
         (isochrones nil))
    
      (log2:info "Routing from ~a to ~a / course angle ~a" start-pos dest-pos dest-heading)

      (do* ( ;; Iteration stops when destination was reached
            (reached nil)
            ;; Iteration stops when stepmax seconds have elapsed
            (stepsum 0
                     (+ stepsum step-size))
            (stepnum 0
                     (1+ stepnum))
            (pointnum 0)
            (elapsed0 (now))
            ;; The initial isochrone is just the start point, heading towards destination
            (isochrone (list (make-routepoint :position start-pos
                                              :heading  dest-heading
                                              :destination-distance (course-distance start-pos dest-pos)))
                       next-isochrone)
            ;; The next isochrone - in addition we collect all hourly isochrones
            (successors-per-point (1+ (truncate (* (routing-fan routing) 2) angle-increment)))
            (next-isochrone (make-array (* successors-per-point (length isochrone)))
                            (make-array (* successors-per-point (length isochrone))))
            (index 0 0)
            ;; Get min and max heading of the point of each isochrone for sorting
            (min-heading 360 360)
            (max-heading 0 0)
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
            (let* ((elapsed (timestamp-difference (now) elapsed0))
                   (pps (/ pointnum elapsed)))
              (log2:info "Examined ~a routepoints in ~2$s (~2$p/s)" pointnum elapsed pps))
            (make-routeinfo :tracks (extract-tracks
                                     (construct-route isochrone))
                            :isochrones isochrones))
        (log2:info "Isochrone ~a at ~a, ~a points" stepnum step-time (length isochrone))
        ;; Iterate over each point in the current isochrone
        (map nil
             (lambda (routepoint)
               (let* ((dest-angle (normalize-angle
                                   (round
                                    (course-angle (routepoint-position routepoint) dest-pos))))
                      (left (normalize-heading (- dest-angle (routing-fan routing))))
                      (right (normalize-heading (+ dest-angle (routing-fan routing)))))
                 ;; Keep track of min and max heading of point in isochrone
                 ;; -- Could be optimzed, don't the firt and last point always provide extreme headings?
                 (when (< left min-heading)
                   (setf min-heading left))
                 (when (> right max-heading)
                   (setf max-heading right))
                 (when (> left right)
                   (incf right 360))
                 (loop
                    :for heading-index :from left :to right :by angle-increment
                    :for heading = (normalize-heading heading-index)
                    :do (multiple-value-bind (speed angle sail)
                            (heading-boatspeed forecast polars-name (routepoint-position routepoint) heading)
                          (when (or (not (equal sail (routepoint-sail routepoint)))
                                    (and (< angle 0 (routepoint-twa routepoint)))
                                    (and (< (routepoint-twa routepoint) 0 angle)))
                            ;; Manoeuvering penalty
                            (setf speed (* speed 0.9)))
                          (let*
                              ((new-pos (add-distance-exact (routepoint-position routepoint)
                                                            (* speed step-size)
                                                            (coerce heading 'double-float)))
                               (dtf (course-distance new-pos dest-pos)))
                            (incf pointnum)
                            (setf (aref next-isochrone index)
                                  (make-routepoint :position new-pos
                                                   :heading heading
                                                   :twa angle
                                                   :speed speed
                                                   :sail sail
                                                   :is-land nil ;; compute this after wake pruning
                                                   :predecessor routepoint
                                                   :origin-angle (course-angle start-pos new-pos)
                                                   :destination-angle (course-angle new-pos dest-pos)
                                                   :destination-distance dtf))
                            (incf index)
                            (when (< dtf 1000)
                              (unless reached
                                (log2:info "Reached destination at ~a" step-time)
                                (setf reached t))))))))
             isochrone)
        (setf next-isochrone (filter-isochrone next-isochrone min-heading max-heading max-points))
        ;; Collect hourly isochrones
        (multiple-value-bind (q r) (truncate stepsum 3600)
          (declare (ignore q))
          (when (zerop r)
            (let ((iso (map 'vector #'routepoint-position next-isochrone)))
              (push iso isochrones))))))))

(defun filter-isochrone (isochrone min-heading max-heading max-points)
  (declare (ignore stepnum))
  (let* ((last
          (1- (length isochrone)))
         (a-start
          (routepoint-origin-angle (aref isochrone 0)))
         (a-end
          (routepoint-origin-angle (aref isochrone last)))
         (southbound
          (< a-end a-start))
         (h-end
          (if southbound  (+ a-end 360) a-end))
         (delta-angle
          (/ (- h-end a-start) max-points))
         (a0
          a-start)
         (dmin
          (routepoint-destination-distance (aref isochrone 0)))
         (kmin
          0)
         (result
          (make-array 0 :fill-pointer 0 :adjustable t)))
    (assert (plusp delta-angle))
    (flet ((routepoint-sort-key (routepoint)
             (or (routepoint-sort-angle% routepoint)
                 (setf (routepoint-sort-angle% routepoint)
                       (let ((angle (routepoint-origin-angle routepoint)))
                         (check-type angle angle)
                         (if (and southbound (< angle 0)) (+ angle 360) angle))))))
      (setf isochrone (sort isochrone #'< :key #'routepoint-sort-key))
      (log2:trace "In: ~a, a-start: ~a, a-end: ~a, diff-a: ~a, delta-a: ~a, h-min: ~a, h-max: ~a"
                  last
                  a-start
                  h-end
                  (* delta-angle max-points)
                  delta-angle
                  min-heading
                  max-heading)
      (loop
         :for point :across isochrone
         :for k :from 0
         :for a = (routepoint-origin-angle point)
         :for d = (routepoint-destination-distance point)
         ;; :initially (vector-push-extend (aref isochrone 0) result)
         :do (cond
               ((>= (abs (- a a0)) delta-angle)
                ;; Sector scanned, record best distance...
                (unless (is-land (latlng-lat (routepoint-position (aref isochrone kmin)))
                                 (latlng-lng (routepoint-position (aref isochrone kmin))))
                  (vector-push-extend (aref isochrone kmin) result))
                ;; ... and reset.
                (when (< k last)
                  (setf kmin k)
                  (let ((next-point (aref isochrone kmin)))
                    (setf a0 (routepoint-origin-angle next-point))
                    (setf dmin (routepoint-destination-distance next-point)))))
               ((< (routepoint-destination-distance point) dmin)
                (setf kmin k)
                (setf dmin (routepoint-destination-distance point))))
         :finally (progn
                    ;; (vector-push-extend (aref isochrone last) result)
                    (return result))))))

(defun southbound-p (min-heading max-heading)
  (< min-heading 180 max-heading))
           
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
  (let* ((forecast-bundle (or (get-forecast-bundle (routing-forecast-bundle routing))
                              (get-forecast-bundle 'constant-wind-bundle)))
         (polars-name (routing-polars routing))
         (start-time (now))
         (step-time (routing-stepsize routing))
         (startpos (routing-start routing))
         (destpos (routing-dest routing)))
    ;; Can't modify latlng!
    ;; (gm-to-grib! startpos)
    ;; (gm-to-grib! destpos)
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
              (twa-boatspeed forecast polars-name curpos twa)
            (setf curpos (add-distance-exact curpos (* speed step-time) heading))))))))

(defun twa-heading (wind-dir angle)
  "Compute HEADING resulting from TWA in WIND"
  (normalize-heading (- wind-dir angle)))

(defvar *boat-speed-ht* (make-hash-table :test #'equal))

(defun heading-twa (wind-dir heading)
  "Compute TWA resulting from HEADING in WIND"
  (normalize-angle (- wind-dir heading)))

(defun twa-boatspeed (forecast polars latlon angle)
  (check-type angle angle)
  (multiple-value-bind (wind-dir wind-speed)
      (get-wind-forecast forecast latlon)
    (multiple-value-bind (speed sail)
        (get-max-speed (abs angle) wind-speed polars)
      (values speed
              (twa-heading wind-dir angle)
              sail
              wind-speed))))

(defun heading-boatspeed (forecast polars latlon heading)
  (check-type heading heading)
  (multiple-value-bind (wind-dir wind-speed)
      (get-wind-forecast forecast latlon)
    (let ((angle (heading-twa wind-dir heading)))
      (multiple-value-bind (speed sail)
          (get-max-speed angle wind-speed polars)
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
;;; Can't SETF LATLNG-LNG!
#+()(defun gm-to-grib! (latlng)
  (when (< (latlng-lng latlng) 0)
    (incf (latlng-lng latlng) 360)))  

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
