;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2018-04-15 21:20:59>

;; -- marks
;; -- atan/acos may return #C() => see CLTL
;; -- use CIS ?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Todo: User settings
;;; Wind & Boat
;;; - sail change penalty 
;;; - tack/gybe penalty 
;;; Search parameters
;;; - land check [on]|off
;;; - long-step-threshold 12h|24h (?)
;;; - long-step-value 30min|[60min]
;;; Display
;;; - display isochrones [on]|off
;;; - display tracks on|[off]


(in-package :virtualhelm)

;;; time in seconds
(defconstant +10min+ (* 10 60))
(defconstant +12h+ (* 12 60 60))
(defconstant +24h+ (* 24 60 60))

(defvar *isochrones* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A routing stores the start and destination of the route
;;; and other routing parameters.
        
(defstruct routing
  (forecast-bundle 'noaa-bundle)
  (polars "vo65")
  (starttime nil) ;; NIL or "yyyy-MM-ddThh:mm" (datetime-local format)
  (starttimezone "+01:00") ;; NIL or "yyyy-MM-ddThh:mm" (datetime-local format)
  (options ())
  (minwind t) ;; m/s !!
  (start +lessables+)
  (dest +lacoruna+)
  (mode +max-origin+)
  (fan 90)
  (angle-increment 3)
  (max-points-per-isochrone 300)
  (stepmax +24h+))

(defstruct duration days hours minutes seconds)
(defmethod print-object ((thing duration) stream)
  (format stream "~dd ~2,'0dh ~2,'0dm"
          (duration-days thing)
          (duration-hours thing)
          (duration-minutes thing)))

(defun routing-foils (routing)
  (member "foil" (routing-options routing) :test #'string=))
(defun routing-hull (routing)
  (member "hull" (routing-options routing) :test #'string=))
(defun routing-winches (routing)
  (member "winch" (routing-options routing) :test #'string=))

(defstruct routeinfo best stats tracks isochrones)
(defstruct routestats start duration sails min-wind max-wind min-twa max-twa)
  
(defstruct isochrone center time offset path)

(defstruct twainfo twa heading path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Isochrones are described by sets of routepoints.
;;;
;;; ### Think of a good sorting/data structure to support finding the most advanced point in a sector

(defstruct (routepoint
             (:constructor create-routepoint (predecessor position time heading &optional destination-distance speed sail penalty wind-dir wind-speed (origin-angle 0) (origin-distance 0))))
  predecessor position time heading destination-distance speed sail penalty wind-dir wind-speed origin-angle origin-distance sort-angle%)

(defun routepoint-twa (rp)
  (if (routepoint-wind-dir rp)
      (round (heading-twa (routepoint-wind-dir rp)
                          (routepoint-heading rp)))))

(defmethod print-object ((thing routepoint) stream)
  (format stream " ~a@~a"
          (routepoint-position thing)
          (routepoint-time thing)))

(defun get-route (routing)
  (let* ((start-pos (routing-start routing))
         (dest-pos (normalized-dest-pos routing))
         (forecast-bundle (or (get-forecast-bundle (routing-forecast-bundle routing))
                              (get-forecast-bundle 'constant-wind-bundle)))
         (sails (encode-options (routing-options routing)))
         (polars (get-routing-polars routing))
         (angle-increment (routing-angle-increment routing))
         (max-points (routing-max-points-per-isochrone routing))
         (dest-heading (round (course-angle start-pos dest-pos)))
         (start-time
          ;; Start time NIL is parsed as NOW
          (parse-datetime-local (routing-starttime routing)
                                :timezone (routing-starttimezone routing))) 
         (isochrones nil))
    (log2:info "Routing from ~a to ~a / course angle ~a searching +/-~a"
               start-pos
               dest-pos
               dest-heading
               (routing-fan routing))
    (flet ((initial-isochrone ()
             (list (create-routepoint nil start-pos start-time dest-heading (course-distance start-pos dest-pos)))))

      (do* ( ;; Iteration stops when destination was reached
            (reached nil)
            (error nil)
            ;; Iteration stops when stepmax seconds have elapsed
            (stepnum 0 (1+ stepnum))
            (stepsum 0 (+ stepsum step-size))
            (step-size (step-size start-time)
                       (step-size start-time step-time))
            (pointnum 0)
            (elapsed0 (now))
            ;; The initial isochrone is just the start point, heading towards destination
            (isochrone (initial-isochrone)
                       next-isochrone)
            ;; The next isochrone - in addition we collect all hourly isochrones
            (next-isochrone (make-array 0 :adjustable t :fill-pointer 0)
                            (make-array 0 :adjustable t :fill-pointer 0))
            ;; Get min and max heading of the point of each isochrone for sorting
            (min-heading 360 360)
            (max-heading 0 0)
            ;; Advance the simulation time AFTER each iteration - this is most likely what GE does
            (step-time (adjust-timestamp start-time (:offset :sec step-size))
                       (adjust-timestamp step-time (:offset :sec step-size)))
            ;; Get wind data for simulation time
            (forecast (get-forecast forecast-bundle step-time)
                      (get-forecast forecast-bundle step-time)))
        
           ;; When the maximum number of iterations is reached, construct the best path
           ;; from the most advanced point's predecessor chain.
           ((or reached
                error
                (>= stepsum (routing-stepmax routing)))
            (log-stats elapsed0 stepnum pointnum)
            (let ((best-route (construct-route isochrone)))
              (setf *isochrones*  isochrones)
              (make-routeinfo :best best-route
                              :stats (get-statistics best-route)
                              :tracks (extract-tracks isochrone)
                              :isochrones (strip-routepoints isochrones))))
        
        (log2:info "Isochrone ~a at ~a, ~a points" stepnum (format-datetime nil step-time) (length isochrone))

        ;; Step 1 - Compute next isochrone by exploring from each point in the current isochrone.
        ;;          Add new points to next-isochrone. 
        (map nil (lambda (rp)
                   (let ((new-point-num
                          (expand-routepoint routing rp dest-heading start-pos step-size step-time forecast polars angle-increment next-isochrone)))
                     (incf pointnum new-point-num)))
             isochrone)
        
        ;; Step 2 - Filter isochrone. 
        (let ((candidate (filter-isochrone next-isochrone max-points :criterion (routing-mode routing))))
          (cond
            ((and candidate (> (length candidate) 0))
             (loop
                :for p :across candidate
                :when p
                :do (progn
                      (setf (routepoint-destination-distance p)
                            (course-distance (routepoint-position p) dest-pos))
                      (when (< (routepoint-destination-distance p) 10000)
                        (setf reached t))))
             (setf next-isochrone candidate))
            (t
             (setf error t)))
          (when reached
            (log2:info "Reached destination at ~a" step-time)))
        ;; Collect hourly isochrones
        (multiple-value-bind (q r) (truncate (timestamp-to-universal step-time) 3600)
          (declare (ignore q))
          (when (zerop r)
            (let* ((start-offset (/ (timestamp-difference start-time (fcb-time forecast-bundle)) 3600))
                   (iso (make-isochrone :center start-pos
                                       :time step-time
                                       :offset (truncate (+ start-offset (/ stepsum 3600)) 1.0)
                                       :path (extract-points next-isochrone))))
              (push iso isochrones))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stepping

(defun step-size (start-time &optional (step-time nil))
  (cond
    ((null step-time)
     ;; First step - runs up to full 10min 
     (let* ((time (timestamp-to-unix start-time)))
       (- (* 600 (truncate (+ time 600) 600))
          time)))
    (t
     (let ((delta-t (timestamp-difference step-time (timestamp-maximize-part start-time :hour))))
       (cond ((<= delta-t (* 36 600))
              600)
             ((<= delta-t (* 72 600))
              1200)
             ((<= delta-t (* 144 600))
              1800)
             (t
              1800))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Speed

(defun get-penalized-avg-speed (routing cur-twa cur-sail wind-dir wind-speed polars heading)
  (multiple-value-bind (speed twa sail)
      (heading-boatspeed polars wind-dir wind-speed heading)
    (when (routing-minwind routing)
      (setf speed (max 2.0578d0 speed)))
    (when
        ;; Foiling speed if twa and tws (in m/s) falls in the specified range
        (routing-foils routing)
      (setf speed (* speed (foiling-factor speed twa))))
    (when (routing-hull routing)
      (setf speed (* speed 1.003)))
    (let ((pspeed
           (if (routing-winches routing)
               (* speed 0.9375)
               (* speed 0.75))))
      (cond
        ((and
          (not (equal sail cur-sail))
          (not (equal twa cur-twa)))
         (values twa sail pspeed "Sail Change" wind-dir wind-speed))
        ((or (< twa 0 cur-twa)
             (< cur-twa 0 twa))
         (values twa sail pspeed "Tack/Gybe" wind-dir wind-speed))
        (t
         (values twa sail speed nil wind-dir wind-speed))))))

(defvar +foil-speeds+ (map 'vector #'knots-to-m/s
                           #(0.0 11.0 16.0 35.0 40.0 70.0)) )
(defvar +foil-angles+ #(0.0 70.0 80.0 160.0 170.0 180.0))
(defvar +foil-matrix+ #2a((1.00 1.00 1.00 1.00 1.00 1.00)
                          (1.00 1.00 1.00 1.00 1.00 1.00)
                          (1.00 1.00 1.04 1.04 1.00 1.00)
                          (1.00 1.00 1.04 1.04 1.00 1.00)
                          (1.00 1.00 1.00 1.00 1.00 1.00)
                          (1.00 1.00 1.00 1.00 1.00 1.00)))
(defun foiling-factor (speed twa)
  (multiple-value-bind
        (speed-index speed-fraction)
      (fraction-index speed +foil-speeds+)
    (multiple-value-bind
          (angle-index angle-fraction)
        (fraction-index (abs twa) +foil-angles+)
      (bilinear-unit speed-fraction
                     angle-fraction
                     (aref +foil-matrix+ speed-index angle-index)
                     (aref +foil-matrix+ (1+ speed-index) angle-index)
                     (aref +foil-matrix+ speed-index (1+ angle-index))
                     (aref +foil-matrix+ (1+ speed-index) (1+ angle-index))))))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aux functions

(defun log-stats (elapsed0 stepnum pointnum)
  (let* ((elapsed (timestamp-difference (now) elapsed0)))
    (log2:info "Elapsed ~2$, Positions ~a, Isochrones ~a | p/s=~2$ | s/i=~4$ | tpi=~2$ |"
               elapsed
               pointnum
               stepnum
               (/ pointnum elapsed)
               (/ elapsed stepnum)
               (coerce (/ pointnum stepnum) 'float))))

(defun expand-routepoint (routing routepoint dest-heading start-pos step-size step-time forecast polars angle-increment next-isochrone)
  (cond
    ((null routepoint)
     (return-from expand-routepoint 0))
    (t
     (let* ((left (normalize-heading (- dest-heading (routing-fan routing))))
            (right (normalize-heading (+ dest-heading (routing-fan routing))))
            (cur-twa (routepoint-twa routepoint))
            (cur-sail (routepoint-sail routepoint)))
       (when (> left right)
         (incf right 360))
       (multiple-value-bind (wind-dir wind-speed)
           (get-wind-forecast forecast (routepoint-position routepoint))
         (loop
            :for heading-index :from left :to right :by angle-increment
            :for heading = (normalize-heading heading-index)
            :for pointnum :from 0
            :do (multiple-value-bind (twa sail speed reason wind-dir wind-speed)
                    (get-penalized-avg-speed routing cur-twa cur-sail wind-dir wind-speed polars heading)
                  (when (or (<= -165 twa -40)
                            (<= 40 twa 165))
                    (let
                        ((new-pos (add-distance-exact (routepoint-position routepoint)
                                                      (* speed step-size)
                                                      (coerce heading 'double-float))))
                      (incf pointnum)
                      (vector-push-extend
                       (construct-rp routepoint start-pos new-pos step-time heading speed sail reason wind-dir wind-speed)
                       next-isochrone))))
            :finally (return pointnum)))))))

(defun construct-rp (previous start-pos position step-time heading speed sail reason wind-dir wind-speed)
  (create-routepoint previous
                     position
                     step-time
                     heading
                     nil
                     speed
                     sail
                     reason
                     wind-dir
                     wind-speed
                     (course-angle start-pos position)
                     (course-distance start-pos position)))

(defun get-routing-polars (routing)
  (let ((sails (encode-options (routing-options routing))))
    (get-combined-polars (routing-polars routing) sails)))

(defun normalized-dest-pos (routing)
  (if (>= (- (latlng-lng (routing-dest routing))
             (latlng-lng (routing-start routing)))
          180)
      (make-latlng :lat (latlng-lat (routing-dest routing))
                   :lng (- (latlng-lng (routing-dest routing)) 360))
      (routing-dest routing)))

(defun strip-routepoints (isochrones)
  (loop
     :for i :in isochrones
     :collect (make-isochrone :time (isochrone-time i)
                              :offset (isochrone-offset i)
                              :path (loop
                                       :for r :across (isochrone-path i)
                                       :collect (routepoint-position r)))))

(defun extract-points (isochrone)
  (let ((points (loop :for p :across isochrone :when p :collect p)))
    (make-array (length points) :initial-contents points)))

(defun extract-tracks (isochrone)
  (loop
     :for point :across isochrone
     :for k :from 0
     :collect (do ((p point (routepoint-predecessor p))
                   (v (list)))
                  ((null p)
                   v)
                (push (routepoint-position p) v))))

(defun construct-route (isochrone)
  (let ((min-dtf nil)
        (min-point nil)
        (route nil))
    (loop
       :for point :across (extract-points isochrone)
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

(defun get-statistics (track)
  (let ((sails nil)
        (min-wind 100)
        (max-wind 0)
        (min-twa 180)
        (max-twa 0))
    (dolist (point track)
      (when (routepoint-sail point)
        (pushnew (routepoint-sail point) sails))
      (setf min-wind
            (min (m/s-to-knots
                  (or (routepoint-wind-speed point) 100))
                 min-wind))
      (setf max-wind
            (max (m/s-to-knots
                  (or (routepoint-wind-speed point) 0)) max-wind))
      (setf min-twa
            (min (abs (or (routepoint-twa point) 180)) min-twa))
      (setf max-twa
            (max (abs (or (routepoint-twa point) 0)) max-twa)))
    (make-routestats :start (routepoint-time (first track))
                     :duration (encode-duration
                                (timestamp-difference (routepoint-time (car (last track)))
                                                      (routepoint-time (first track))))
                     :sails sails
                     :min-wind min-wind
                     :max-wind max-wind
                     :min-twa min-twa
                     :max-twa max-twa)))

(defun encode-duration (seconds)
  (multiple-value-bind (days rest-hours)
      (truncate seconds 86400)
    (multiple-value-bind (hours rest-minutes)
        (truncate rest-hours 3600)
      (multiple-value-bind (minutes rest-seconds)
          (truncate rest-minutes 60)
        (make-duration :days days
                       :hours hours
                       :minutes minutes
                       :seconds rest-seconds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TWA tool

(defun get-twa-path (routing
                     &key
                       time
                       lat-a
                       lng-a
                       lat
                       lng
                       (total-time +12h+)
                       (step-num (truncate total-time +10min+)))
  (let* ((forecast-bundle (or (get-forecast-bundle (routing-forecast-bundle routing))
                              (get-forecast-bundle 'constant-wind-bundle)))
         (sails (encode-options (routing-options routing)))
         (polars (get-combined-polars (routing-polars routing) sails))
         (start-time (or time (now)))
         (step-time +10min+)
         (startpos (make-latlng :lat lat-a :lng lng-a)))
    (let* ((heading (course-angle startpos (make-latlng :lat lat :lng lng)))
           (curpos (copy-latlng startpos))
           (wind-dir (get-wind-forecast (get-forecast forecast-bundle start-time) startpos))
           (twa (coerce (round (heading-twa wind-dir heading)) 'double-float))
           (path nil))
      (dotimes (k
                 step-num
                (make-twainfo :twa twa
                              :heading (normalize-heading
                                        (round heading))
                              :path (reverse (push (copy-latlng curpos) path))))
        ;; Save current position
        (push (copy-latlng curpos) path)
        (adjust-timestamp! start-time (:offset :sec step-time))
        (let ((forecast (get-forecast forecast-bundle start-time)))
          (multiple-value-bind (speed heading)
              (twa-boatspeed forecast polars curpos twa)
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
    (destructuring-bind (speed sail)
        (get-max-speed polars angle wind-speed)
      (values speed
              (twa-heading wind-dir angle)
              sail
              wind-speed))))

(defun heading-boatspeed (polars wind-dir wind-speed heading)
  (check-type heading heading)
  (let ((angle (heading-twa wind-dir heading)))
    (destructuring-bind (speed sail)
        (get-max-speed polars angle wind-speed)
      (values speed angle sail))))

(defun parse-datetime-local (time &key (timezone "+01:00"))
  (etypecase time
    (null
     (now))
    (timestamp
     time)
    (string
     (parse-rfc3339-timestring (format () "~a:00~a" time timezone)))))

;;; Translate latitude coordinates of start and destination to [0..360)
;;; Can't SETF LATLNG-LNG!
#+()(defun gm-to-grib! (latlng)
  (when (< (latlng-lng latlng) 0)
    (incf (latlng-lng latlng) 360)))  

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
