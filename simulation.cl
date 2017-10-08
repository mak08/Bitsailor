;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-10-07 23:23:22>

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
(defconstant +10min+ (* 10 60))
(defconstant +20min+ (* 20 60))
(defconstant +30min+ (* 30 60))
(defconstant +60min+ (* 60 60))
(defconstant +3h+ (* 3 60 60))
(defconstant +6h+ (* 6 60 60))
(defconstant +12h+ (* 12 60 60))
(defconstant +24h+ (* 24 60 60))
(defconstant +48h+ (* 48 60 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A routing stores the start and destination of the route as well as
;;; routing parameters

(defvar +LACORUNA+
  (make-latlng :lat 43.484812d0 :lng -8.5897636d0))

(defvar +LESSABLES+
  (make-latlng :lat 46.479120d0 :lng -1.7941526d0))

(defvar +FEHMARN+
  (make-latlng :lat 54.434403d0 :lng 11.361632d0))

(defvar +YSTAD+
  (make-latlng :lat 55.391123d0 :lng 13.792635d0))
        
(defstruct routing
  (forecast-bundle 'dwd-icon-bundle)
  (polars "VO65")
  (foils t)
  (fastmanoeuvres t)
  (minwind t) ;; m/s !!
  (start +lessables+)
  (dest +lacoruna+)
  (fan 75)
  (angle-increment 5)
  (max-points-per-isochrone 100)
  (stepmax +24h+)
  (stepsize +10min+))

(defstruct routeinfo best tracks isochrones)

(defstruct isochrone time offset path)

(defstruct twainfo twa path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Isochrones are described by sets of routepoints.
;;;
;;; ### Think of a good sorting/data structure to support finding the most advanced point in a sector

(defstruct routepoint
  position
  time
  heading
  twa
  speed
  penalty
  sail
  wind-dir
  wind-speed
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
            (start-time (now))
            (start-offset (/ (timestamp-difference start-time (fcb-time forecast-bundle)) 3600))
            (elapsed0 start-time)
            ;; The initial isochrone is just the start point, heading towards destination
            (isochrone (list (make-routepoint :position start-pos
                                              :time (format-rfc3339-timestring nil elapsed0)
                                              :heading  dest-heading
                                              :destination-distance (course-distance start-pos dest-pos)))
                       next-isochrone)
            ;; The next isochrone - in addition we collect all hourly isochrones
            (successors-per-point (1+ (truncate (* (routing-fan routing) 2) angle-increment)))
            (next-isochrone (make-array 0 :adjustable t :fill-pointer 0)
                            (make-array 0 :adjustable t :fill-pointer 0))
            (index 0 0)
            ;; Get min and max heading of the point of each isochrone for sorting
            (min-heading 360 360)
            (max-heading 0 0)
            ;; Advance the simulation time BEFORE each iteration - this is most likely what GE does
            (step-time (adjust-timestamp start-time (:offset :sec step-size))
                       (adjust-timestamp step-time (:offset :sec step-size)))
            (step-time-string (format-rfc3339-timestring nil step-time)
                              (format-rfc3339-timestring nil step-time))
            ;; Get wind data for simulation time
            (forecast (get-forecast forecast-bundle step-time)
                      (get-forecast forecast-bundle step-time)))
        
           ;; When the maximum number of iterations is reached, construct the best path
           ;; from the most advanced point's predecessor chain.
           ((or reached
                (>= stepsum (routing-stepmax routing)))
            (let* ((elapsed (timestamp-difference (now) elapsed0)))
              (log2:info "Elapsed ~2$, Positions ~a, Isochrones ~a | p/s=~2$ | s/i=~4$ | tpi=~2$ |"
                         elapsed
                         pointnum
                         stepnum
                         (/ pointnum elapsed)
                         (/ elapsed stepnum)
                         (coerce (/ pointnum stepnum) 'float)))

            (make-routeinfo :best (construct-route isochrone)
                            :tracks (extract-tracks isochrone)
                            :isochrones isochrones))
        (log2:info "Isochrone ~a at ~a, ~a points" stepnum step-time-string (length isochrone))
        ;; Iterate over each point in the current isochrone
        (map nil
             (lambda (routepoint)
               (let* ((dest-angle (normalize-angle
                                   (round
                                    (course-angle (routepoint-position routepoint) dest-pos))))
                      (left (normalize-heading (- dest-angle (routing-fan routing))))
                      (right (normalize-heading (+ dest-angle (routing-fan routing)))))
                 ;; Keep track of min and max heading of point in isochrone
                 (when (< left min-heading)
                   (setf min-heading left))
                 (when (> right max-heading)
                   (setf max-heading right))
                 (when (> left right)
                   (incf right 360))
                 (loop
                    :for heading-index :from left :to right :by angle-increment
                    :for heading = (normalize-heading heading-index)
                    :do (multiple-value-bind (twa sail speed reason wind-dir wind-speed)
                            (get-penalized-avg-speed routing routepoint forecast polars-name heading)
                          (when (or (<= -175 twa -40)
                                    (<= 40 twa 175))
                            (let*
                                ((new-pos (add-distance-exact (routepoint-position routepoint)
                                                              (* speed step-size)
                                                              (coerce heading 'double-float)))
                                 (dtf (course-distance new-pos dest-pos)))
                              (incf pointnum)
                              (vector-push-extend
                               (make-routepoint :position new-pos
                                                :time step-time-string
                                                :heading heading
                                                :twa (round twa)
                                                :speed speed
                                                :penalty reason
                                                :sail sail
                                                :wind-dir wind-dir
                                                :wind-speed wind-speed
                                                :is-land nil ;; compute this after filtering
                                                :predecessor routepoint
                                                :origin-angle (course-angle start-pos new-pos)
                                                :destination-angle "not computed"
                                                :destination-distance dtf)
                               next-isochrone)
                              (incf index)
                              (when (< dtf 10000)
                                (unless reached
                                  (log2:info "Reached destination at ~a" step-time)
                                  (setf reached t)))))))))
             isochrone)
        (setf next-isochrone (filter-isochrone next-isochrone min-heading max-heading max-points))
        ;; Collect hourly isochrones
        (multiple-value-bind (q r) (truncate stepsum 3600)
          (declare (ignore q))
          (when (zerop r)
            (let ((iso (make-isochrone :time (to-rfc3339-timestring step-time)
                                       :offset (+ start-offset (/ (* stepnum step-size) 3600))
                                       :path (map 'vector #'routepoint-position next-isochrone))))
              (push iso isochrones))))))))

(defun get-penalized-avg-speed (routing predecessor forecast polars-name heading)
  (multiple-value-bind (speed twa sail wind-dir wind-speed)
      (heading-boatspeed forecast polars-name (routepoint-position predecessor) heading)
    (when (routing-minwind routing)
      (setf speed (max 2.0578d0 speed)))
    (when
        ;; Foiling speed if twa and tws (in m/s) falls in the specified range
        (and (routing-foils routing)
             (< 80 twa 160)
             (< 8.23 wind-speed 18.00))
      (setf speed (* speed 1.04)))
    (let ((pspeed
           (if (routing-fastmanoeuvres routing)
               (* speed 0.9375)
               (* speed 0.75))))
      (cond
        ((and
          (not (equal sail (routepoint-sail predecessor)))
          (not (equal twa (routepoint-twa predecessor))))
         (values twa sail pspeed "Sail Change" wind-dir wind-speed))
        ((or (< twa 0 (routepoint-twa predecessor))
             (< (routepoint-twa predecessor) 0 twa))
         (values twa sail pspeed "Tack/Gybe" wind-dir wind-speed))
        (t
         (values twa sail speed nil wind-dir wind-speed))))))

(defun filter-isochrone (isochrone min-heading max-heading max-points)
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
  (let ((min-dtf nil)
        (min-point nil)
        (route nil))
    (loop
       :for point :across isochrone
       :do (when (or (null min-point)
                     (< (routepoint-destination-distance point) min-dtf))
             (setf min-dtf (routepoint-destination-distance point)
                   min-point point)))
    (do ((cur-point min-point (routepoint-predecessor cur-point))
         (predecessor nil cur-point))
        ((null cur-point)
         route)
      (when (or (null predecessor)
                (not (eql (routepoint-heading cur-point) (routepoint-heading predecessor)))
                (not (eql (routepoint-sail cur-point) (routepoint-sail predecessor))))
        (let ((next-point (copy-routepoint cur-point)))
          (setf (routepoint-predecessor next-point) nil)
          (push next-point route))))))

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
                       lat-a
                       lng-a
                       lat
                       lng
                       (total-time +12h+)
                       (step-num (truncate total-time (routing-stepsize routing))))
  (let* ((forecast-bundle (or (get-forecast-bundle (routing-forecast-bundle routing))
                              (get-forecast-bundle 'constant-wind-bundle)))
         (polars-name (routing-polars routing))
         (start-time (now))
         (step-time (routing-stepsize routing))
         (startpos (make-latlng :lat lat-a :lng lng-a)))
    ;; Can't modify latlng!
    ;; (gm-to-grib! startpos)
    ;; (gm-to-grib! destpos)
    (let* ((heading (course-angle startpos (make-latlng :lat lat :lng lng)))
           (curpos (copy-latlng startpos))
           (wind-dir (get-wind-forecast (get-forecast forecast-bundle start-time) startpos))
           (twa (coerce (round (heading-twa wind-dir heading)) 'double-float))
           (path nil))
      (dotimes (k
                 step-num
                (make-twainfo :twa twa
                              :path (reverse (push (copy-latlng curpos) path))))
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
        (values speed angle sail wind-dir wind-speed)))))

;;; Translate latitude coordinates of start and destination to [0..360)
;;; Can't SETF LATLNG-LNG!
#+()(defun gm-to-grib! (latlng)
  (when (< (latlng-lng latlng) 0)
    (incf (latlng-lng latlng) 360)))  

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
