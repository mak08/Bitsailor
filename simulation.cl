;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2018-12-04 00:59:08>

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
(defvar *best-route*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A routing stores the start and destination of the route
;;; and other routing parameters.
        
(defstruct routing
  (forecast-bundle 'noaa-bundle)
  (polars "vo65")
  (starttime nil) ;; NIL or "yyyy-MM-ddThh:mm" (datetime-local format)
  (starttimezone "+00:00") ;; NIL or "yyyy-MM-ddThh:mm" (datetime-local format)
  (options '("reach"))
  (minwind t) ;; m/s !!
  (start +lessables+)
  (dest +lacoruna+)
  (mode +max-origin+)
  (fan 90)
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

(defmethod print-object ((thing routeinfo) stream)
  (let ((stats (or (routeinfo-stats thing)
                   (make-routestats))))
    (format stream "#<RouteInfo Start ~a Duration ~a Isochrones ~a>"
            (routestats-start stats)
            (routestats-duration stats)
            (length (routeinfo-isochrones thing)))))

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
         (polars (get-routing-polars routing))
         (dest-heading (round (normalize-heading (course-angle start-pos dest-pos))))
         (left (normalize-heading (- dest-heading (routing-fan routing))))
         (right (normalize-heading (+ dest-heading (routing-fan routing))))
         (start-time
          ;; Start time NIL is parsed as NOW
          (parse-datetime-local (routing-starttime routing)
                                :timezone "+00:00")) 
         (isochrones nil))
    (log2:info "Routing from ~a to ~a / course angle ~a searching +/-~a"
               start-pos
               dest-pos
               dest-heading
               (routing-fan routing))
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
          ;; Increase max-points per isochrone as the isochrones expand to keep resolution roughly constant
          (max-points 200 (min 1500 (+ max-points 5)))
          ;; Advance the simulation time AFTER each iteration - this is most likely what GE does
          (step-time (adjust-timestamp start-time (:offset :sec step-size))
                     (adjust-timestamp step-time (:offset :sec step-size)))
          ;; Get wind data for simulation time
          (forecast (get-forecast forecast-bundle step-time)
                    (get-forecast forecast-bundle step-time))
          ;; The initial isochrone is just the start point, heading towards destination
          (isochrone
           (multiple-value-bind (wind-dir wind-speed) 
               (get-wind-forecast forecast start-pos)
             (make-array 1 :initial-contents
                         (list
                          (create-routepoint nil start-pos start-time dest-heading (course-distance start-pos dest-pos) nil nil nil wind-dir  wind-speed)))))
          ;; The next isochrone - in addition we collect all hourly isochrones
          (next-isochrone (make-array 0 :adjustable t :fill-pointer 0)
                          (make-array 0 :adjustable t :fill-pointer 0)))
        
         ;; When the maximum number of iterations is reached, construct the best path
         ;; from the most advanced point's predecessor chain.
         ((or reached
              error
              (>= stepsum (routing-stepmax routing)))
          (log-stats elapsed0 stepnum pointnum)
          (let ((best-route (construct-route isochrone dest-pos)))
            (setf *best-route* best-route)
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
                        (expand-routepoint routing rp start-pos left right step-size step-time forecast polars next-isochrone)))
                   (incf pointnum new-point-num)))
           isochrone)
        
      ;; Step 2 - Filter isochrone. 
      (let ((candidate (filter-isochrone next-isochrone left right max-points :criterion (routing-mode routing))))
        (cond
          ((or (null candidate)
               (= (length candidate) 0)
               (notany #'routepoint-p candidate))
           (setf error t))
          (t
           (setf reached (reached candidate dest-pos))
           (setf isochrone candidate)
           ;; Collect hourly isochrones
           (multiple-value-bind (q r) (truncate (timestamp-to-universal step-time) 3600)
             (declare (ignore q))
             (when (zerop r)
               (let* ((start-offset (/ (timestamp-difference start-time (fcb-time forecast-bundle)) 3600))
                      (iso (make-isochrone :center start-pos
                                      :time step-time
                                      :offset (truncate (+ start-offset (/ stepsum 3600)) 1.0)
                                      :path (extract-points isochrone))))
                 (push iso isochrones))))))
        (when reached
          (log2:info "Reached destination at ~a" step-time))))))

(defun reached (candidate dest-pos)
  (some (lambda (p)
          (and p
               (< (course-distance (routepoint-position p) dest-pos) 10000)))
        candidate))

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
     (let ((delta-t (timestamp-difference step-time (timestamp-maximize-part start-time :hour :timezone +utc-zone+))))
       (cond ((<= delta-t (* 36 600))
              300)
             ((<= delta-t (* 72 600))
              900)
             ((<= delta-t (* 144 600))
              1800)
             (t
              1800))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Speed

(defun get-penalized-avg-speed (routing cur-twa cur-sail wind-dir wind-speed polars twa)
  (multiple-value-bind (speed heading sail)
      (twa-boatspeed polars wind-dir wind-speed (normalize-angle twa))
    (when (routing-minwind routing)
      (setf speed (max 1.0289d0 speed)))
    (when
        ;; Foiling speed if twa and tws (in m/s) falls in the specified range
        (routing-foils routing)
      (setf speed (* speed (foiling-factor speed twa))))
    (when (routing-hull routing)
      (setf speed (* speed 1.003)))
    (let ((penalty
           (if (routing-winches routing) 0.9375 0.75)))
      (cond
        ((and
          (not (equal sail cur-sail))
          (not (equal twa cur-twa)))
         (values sail (* speed penalty) "Sail Change"))
        ((or (< twa 0 cur-twa)
             (< cur-twa 0 twa))
         (values sail (* speed penalty) "Tack/Gybe"))
        (t
         (values sail speed nil))))))

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
                     (aref +foil-matrix+ speed-index (1+ angle-index))
                     (aref +foil-matrix+ (1+ speed-index) angle-index)
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

(defun expand-routepoint (routing routepoint start-pos left right step-size step-time forecast polars next-isochrone)
  (cond
    ((null routepoint)
     (return-from expand-routepoint 0))
    ((not (and
           (<= -75d0 (latlng-lat (routepoint-position routepoint)) 85d0)
           (<= -180d0 (latlng-lng (routepoint-position routepoint)) 180d0)))
     (return-from expand-routepoint 0))
    (t
     (let* ((cur-twa (routepoint-twa routepoint))
            (cur-sail (routepoint-sail routepoint))
            (twa-points (cpolars-twa polars))
            (all-twa-points (make-array (length twa-points)
                                        :initial-contents twa-points
                                        :adjustable t
                                        :fill-pointer t)))
       (with-bindings
           (((wind-dir wind-speed)
             (get-wind-forecast forecast (routepoint-position routepoint)))
            ((up-vmg down-vmg) (best-vmg polars wind-speed )))
         (vector-push-extend (third up-vmg) all-twa-points)
         (vector-push-extend (third down-vmg) all-twa-points)
         (loop
            :with up-vmg-angle = (third up-vmg)
            :with down-vmg-angle = (third down-vmg)
            :for pointnum :from 0
            :for twa :across all-twa-points
            :for heading-stbd = (twa-heading wind-dir twa)
            :for heading-port = (twa-heading wind-dir (- twa))
            :when (and (> twa 0)
                       (<= up-vmg-angle twa down-vmg-angle)
                       (or (between-heading left right heading-stbd)
                           (between-heading left right heading-port)))
            :do (flet ((add-point (heading twa)
                         (multiple-value-bind (sail speed reason)
                             (get-penalized-avg-speed routing cur-twa cur-sail wind-dir wind-speed polars twa)
                           (let
                               ((new-pos (add-distance-estimate (routepoint-position routepoint)
                                                                (* speed step-size)
                                                                (coerce heading 'double-float))))
                             (incf pointnum)
                             (vector-push-extend
                              (construct-rp routepoint start-pos new-pos step-time heading speed sail reason wind-dir wind-speed)
                              next-isochrone)))))
                  (when (between-heading left right heading-stbd)
                    (add-point heading-stbd twa))
                  (when (between-heading left right heading-port)
                    (add-point heading-port (- twa))))
            :finally (return pointnum)))))))

(defun between-heading (left right heading)
  (cond ((<= left right)
         (<= left heading right))
        (t
         (or
          (<= left heading 360)
          (<= 0 heading right)))))
    
(defun construct-rp (previous start-pos position step-time heading speed sail reason wind-dir wind-speed)
  (let ((distance (course-distance start-pos position)))
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
                       (course-angle start-pos position distance)
                       distance)))

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
     :when point ; Don't send NULL entries
     :collect (do ((p point (routepoint-predecessor p))
                   (v (list)))
                  ((null p)
                   v)
                (push (routepoint-position p) v))))

(defun construct-route (isochrone dest-pos)
  (let ((min-dtf nil)
        (min-point nil)
        (route nil))
    (loop
       :for point :across (extract-points isochrone)
       :do (progn
             (setf (routepoint-destination-distance point)
                   (course-distance (routepoint-position point) dest-pos))
             (when (or (null min-point)
                       (< (routepoint-destination-distance point) min-dtf))
               (setf min-dtf (routepoint-destination-distance point)
                     min-point point))))
    (do ((cur-point min-point (routepoint-predecessor cur-point))
         (predecessor nil cur-point))
        ((null cur-point)
         route)
      (when (or (null predecessor)
                (not (eql (routepoint-twa cur-point) (routepoint-twa predecessor)))
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
         (time (or time (now)))
         (time-increment +10min+)
         (startpos (make-latlng :lat lat-a :lng lng-a)))
    (let* ((heading (course-angle startpos (make-latlng :lat lat :lng lng)))
           (curpos (copy-latlng startpos))
           (wind-dir (get-wind-forecast (get-forecast forecast-bundle time) startpos))
           (twa (coerce (round (heading-twa wind-dir heading)) 'double-float))
           (path nil))
      (dotimes (k
                 step-num
                (make-twainfo :twa twa
                              :heading (normalize-heading
                                        (round heading))
                              :path (reverse (push (list time (copy-latlng curpos)) path))))
        ;; Save current position
        (push (list time (copy-latlng curpos)) path)
        (setf time (adjust-timestamp time (:offset :sec time-increment)))
        (let ((forecast (get-forecast forecast-bundle time)))
          (multiple-value-bind (wind-dir wind-speed)
              (get-wind-forecast forecast curpos)
            (multiple-value-bind (speed heading)
                (twa-boatspeed polars wind-dir wind-speed twa)
              (setf curpos (add-distance-exact curpos (* speed time-increment) heading)))))))))

(defun twa-heading (wind-dir angle)
  "Compute HEADING resulting from TWA in WIND"
  (normalize-heading (- wind-dir angle)))

(defvar *boat-speed-ht* (make-hash-table :test #'equal))

(defun heading-twa (wind-dir heading)
  "Compute TWA resulting from HEADING in WIND"
  (normalize-angle (- wind-dir heading)))

(defun twa-boatspeed (polars wind-dir wind-speed angle)
  (check-type angle angle)
  (destructuring-bind (speed sail)
      (get-max-speed (cpolars-speed polars) angle wind-speed)
    (values speed
            (twa-heading wind-dir angle)
            sail)))

(defun heading-boatspeed (polars wind-dir wind-speed heading)
  (check-type heading heading)
  (let ((angle (heading-twa wind-dir heading)))
    (destructuring-bind (speed sail)
        (get-max-speed (cpolars-speed polars) angle wind-speed)
      (values speed angle sail))))

(defun parse-datetime-local (time &key (timezone "+00:00"))
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
