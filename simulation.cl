;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2021-03-28 01:15:30>

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

(defconstant  +max-iso-points+ 1500)

(defconstant +reached-distance+ 30000d0)
(defvar *isochrones* nil)
(defvar *best-route*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-route

(defun get-route (routing)
  (let* ((start-pos (routing-start routing))
         (dest-pos (normalized-dest-pos routing))
         (polars (get-routing-polars routing))
         (race-info (get-routing-race-info routing))
         (limits (get-race-limits race-info))
         (dest-heading (round (course-angle start-pos dest-pos)))
         (left (normalize-heading (coerce (- dest-heading (routing-fan routing)) 'double-float)))
         (right (normalize-heading (coerce (+ dest-heading (routing-fan routing)) 'double-float)))
         (start-time
          ;; (timestamp-maximum (now)
                             (parse-datetime-local (routing-starttime routing)
                                                   :timezone "+00:00")
           ;; )
           )
         (cycle (routing-cycle routing))
         (isochrones nil))
    (log2:info "Routing from ~a to ~a at ~a Course angle ~a Fan ~a"
               (format-latlng nil start-pos)
               (format-latlng nil dest-pos)
               start-time
               dest-heading
               (routing-fan routing))
    (log2:info "Using cycle ~a" cycle)
    (do* ( ;; Iteration stops when destination was reached
          (reached nil)
          (reached-distance (* 900 (knots-to-m/s (cpolars-maxspeed polars))))
          (dummy (log2:info "Max speed: ~,1,,,f Reached distance: ~a"
                            (cpolars-maxspeed polars)
                            reached-distance))
          (error nil)
          (stepnum 1 (1+ stepnum))
          ;; Iteration stops when stepmax seconds have elapsed
          (stepsum 0 (+ stepsum step-size))
          (pointnum 0)
          (elapsed0 (now))
          ;; Increase max-points per isochrone as the isochrones expand to keep resolution roughly constant
          (max-points 200 (min +max-iso-points+ (+ max-points 12)))
          ;; The first step-size and when we apply it is important - brings step-time to mod 10min
          (step-size (step-size start-time)
                     (step-size start-time step-time))
          ;; Get wind data for simulation time
          ;; Advance the simulation time AFTER each iteration - this is most likely what GE does
          (step-time (adjust-timestamp start-time (:offset :sec step-size))
                     (adjust-timestamp step-time (:offset :sec step-size)))
          (params (interpolation-parameters step-time cycle)
                  (interpolation-parameters step-time cycle))
          (base-time (base-time params))
          ;; The initial isochrone is just the start point, heading towards destination
          (isochrone
           (multiple-value-bind (wind-dir wind-speed) 
               (interpolated-prediction (latlng-lat start-pos) (latlng-lng start-pos) params)
             (make-array 1 :initial-contents
                         (list
                          (create-routepoint nil start-pos start-time nil (course-distance start-pos dest-pos) nil nil nil wind-dir wind-speed)))))
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
                            :polars (cpolars-label polars)
                            :maxspeed (cpolars-maxspeed polars)
                            :tracks (extract-tracks start-pos (course-angle start-pos dest-pos) isochrone)
                            :isochrones (prepare-routepoints isochrones))))

      (declare (fixnum max-points step-size stepsum pointnum))

      ;; Step 1 - Compute next isochrone by exploring from each point in the current isochrone.
      ;;          Add new points to next-isochrone. 
      (map nil (lambda (rp)
                 (let ((new-point-num
                         (expand-routepoint routing rp start-pos left right step-size step-time params polars next-isochrone)))
                   (declare (fixnum new-point-num))
                   (incf pointnum new-point-num)))
           isochrone)
      
      ;; Step 2 - Filter isochrone.
      (let ((candidate (filter-isochrone next-isochrone left right max-points
                                         :criterion (routing-mode routing)
                                         :constraints (get-constraints (routing-race-id routing))
                                         :limits limits)))
        (cond
          ((or (null candidate)
               (= (length candidate) 0)
               (notany #'routepoint-p candidate))
           (setf error t))
          (t
           (setf reached (reached candidate dest-pos reached-distance))
           (setf isochrone candidate)

           (log2:trace "Isochrone ~a at ~a, ~a points" stepnum (format-datetime nil step-time) (length isochrone))

           ;; Collect hourly isochrones
           (multiple-value-bind (q r) (truncate (the fixnum (timestamp-to-universal step-time)) 3600)
             (declare (ignore q))
             (when (zerop r)
               (let* (
                      (iso (make-isochrone :center start-pos
                                           :time base-time
                                           :offset (truncate (timestamp-difference step-time base-time) 3600)
                                           :params (null params)
                                           :path (extract-points isochrone))))
                 (push iso isochrones))))))
        (when reached
          (log2:info "Reached destination at ~a" step-time))))))

(defun reached (candidate dest-pos &optional (reached-distance  +reached-distance+))
  (some (lambda (p)
          (and p
               (< (fast-course-distance (routepoint-position p) dest-pos) reached-distance)))
        candidate))

(defun get-race-limits (leg-info)
  (let* ((south (joref (joref leg-info "ice_limits") "south"))
         (result (make-array (+ (length south) 1))))
    (map-into result
              (lambda (p)
                (make-latlng :lat (coerce (joref p "lat") 'double-float)
                             :lng (coerce (joref p "lon") 'double-float)))
              south)
    ;; Wrap around
    (setf (aref result (1- (length result))) (aref result 1))
    result))

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
     (let ((delta-t  (timestamp-difference step-time (timestamp-maximize-part start-time :hour :timezone +utc-zone+))))
       (cond ((<= delta-t (* 12 600))
              600)
             ((<= delta-t (* 48 600))
              600)
             ((<= delta-t (* 72 600))
              900)
             ((<= delta-t (* 144 600))
              1800)
             (t
              1800))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Speed

(declaim (inline foiling-factor))
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

(defun get-penalized-avg-speed (routing cur-twa cur-sail wind-dir wind-speed polars twa)
  (declare (double-float wind-speed twa))
  (when (routing-minwind routing)
    (setf wind-speed (max 1.0289d0 wind-speed)))
  (multiple-value-bind (speed sail)
      (twa-boatspeed polars wind-speed (normalize-angle twa))
    (declare (double-float speed))
    (when
        ;; Foiling speed if twa and tws (in m/s) falls in the specified range
        (routing-foils routing)
      (setf speed (* speed (the double-float (foiling-factor wind-speed twa)))))
    (when (routing-hull routing)
      (setf speed (* speed 1.003d0)))
    (let ((penalty
           (if (routing-winches routing) 0.9375d0 0.75d0)))
      (cond
        ((and
          cur-sail
          (not (equal sail cur-sail)))
         (values (* speed penalty) sail "Sail Change"))
        ((and cur-twa
              (or (< twa 0 cur-twa)
                  (< cur-twa 0 twa)))
         (values (* speed penalty) sail "Tack/Gybe"))
        (t
         (values speed sail nil))))))

(unless (boundp '+foil-speeds+)
(defconstant +foil-speeds+ (map 'vector #'knots-to-m/s
                                #(0.0d0 11.0d0 16.0d0 35.0d0 40.0d0 70.0d0)) )
(defconstant +foil-angles+ #(0.0d0 70.0d0 80.0d0 160.0d0 170.0d0 180.0d0))
(defconstant +foil-matrix+ #2a((1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.04d0 1.04d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.04d0 1.04d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)
                               (1.00d0 1.00d0 1.00d0 1.00d0 1.00d0 1.00d0)))
)

           
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

(declaim (inline twa-heading))
(defun twa-heading (wind-dir angle)
  "Compute HEADING resulting from TWA in WIND"
  (declare (double-float wind-dir angle) (inline normalize-heading))
  (normalize-heading (- wind-dir angle)))

(defun expand-routepoint (routing routepoint start-pos left right step-size step-time params polars next-isochrone)
  (cond
    ((null routepoint)
     (return-from expand-routepoint 0))
    ((not (and
           (<= -75d0 (latlng-lat (routepoint-position routepoint)) 85d0)))
     (return-from expand-routepoint 0))
    (t
     (let* ((cur-twa (routepoint-twa routepoint))
            (cur-sail (routepoint-sail routepoint))
            (twa-points (cpolars-twa polars))
            (all-twa-points (make-array (length twa-points)
                                        :initial-contents twa-points
                                        :adjustable t
                                        :fill-pointer t))
            (lat (latlng-lat (routepoint-position routepoint)))
            (lng (latlng-lng (routepoint-position routepoint))))
       (multiple-value-bind
             (wind-dir wind-speed)
           (interpolated-prediction lat lng params)
         (when (null wind-dir)
           ;; No wind forecast
           (return-from expand-routepoint 0))
         (multiple-value-bind (up-vmg down-vmg)
             (best-vmg polars wind-speed)
           (vector-push-extend (third up-vmg) all-twa-points)
           (vector-push-extend (third down-vmg) all-twa-points)
           (loop
              :with up-vmg-angle = (third up-vmg)
              :with down-vmg-angle = (third down-vmg)
              :for pointnum :from 0
              :for twa :across all-twa-points
              :for heading-stbd = (twa-heading wind-dir twa)
              :for heading-port = (twa-heading wind-dir (- twa))
              :for is-stbd = (heading-between left right heading-stbd)
              :for is-port = (heading-between left right heading-port)
              :when (and (> twa 0)
                         (<= up-vmg-angle twa down-vmg-angle)
                         (or is-stbd is-port))
              :do (flet ((add-point (heading twa)
                           (multiple-value-bind (speed sail reason)
                               (get-penalized-avg-speed routing cur-twa cur-sail wind-dir wind-speed polars twa)
                             (declare (double-float speed)
                                      (integer step-size))
                             (let
                                 ((new-pos (add-distance-estimate (routepoint-position routepoint)
                                                                  (* speed step-size)
                                                                  (coerce heading 'double-float))))
                               (when t
                                 (incf pointnum)
                                 (vector-push-extend
                                  (construct-rp routepoint start-pos new-pos step-time heading speed sail reason wind-dir wind-speed)
                                  next-isochrone))))))
                    (when is-stbd
                      (add-point heading-stbd twa))
                    (when is-port
                      (add-point heading-port (- twa))))
              :finally (return pointnum))))))))


(defun construct-rp (previous start-pos position step-time heading speed sail reason wind-dir wind-speed)
  (let ((distance (or (fast-course-distance start-pos position))))
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
                       (course-angle-d start-pos position distance)
                       distance)))

(defun get-routing-polars (routing)
  (let ((sails (encode-options (routing-options routing))))
    (get-combined-polars (routing-polars routing) sails)))

(defun get-routing-race-info (routing)
  (gethash (routing-race-id routing) *races-ht*))

(defun normalized-dest-pos (routing)
  (if (>= (- (latlng-lng (routing-dest routing))
             (latlng-lng (routing-start routing)))
          180d0)
      (make-latlng :latr% (latlng-latr (routing-dest routing))
                   :lngr% (- (latlng-lngr (routing-dest routing)) (* 2 pi)))
      (routing-dest routing)))

(defun prepare-routepoints (isochrones)
  (loop
     :for i :in (construct-isochrones isochrones)
     :collect (make-isochrone :center (isochrone-center i)
                              :time (isochrone-time i)
                              :offset (isochrone-offset i)
                              :params (null (isochrone-params i))
                              :path (loop
                                       :for r :in (isochrone-path i)
                                       :collect (routepoint-position r)))))

(defun construct-isochrones (isochrones)
  (loop
     :for isochrone :in isochrones
     :append (separate-isochrones isochrone)))

(defun separate-isochrones (isochrone)
  (do* ((path (isochrone-path isochrone))
        (index 0 (1+ index))
        (point (aref path index) (aref path index))
        (previous nil point)
        (current-path nil)
        (isochrones nil))
       ((>= index (1- (length path)))
        (cons (make-isochrone :time (isochrone-time isochrone)
                              :offset (isochrone-offset isochrone)
                              :path current-path)
              isochrones))
    (cond
      ((or (null previous)
           (and (< (abs (- (latlng-lat (routepoint-position point))
                           (latlng-lat (routepoint-position previous))))
                   0.5)
                (< (abs (- (latlng-lng (routepoint-position point))
                           (latlng-lng (routepoint-position previous))))
                   0.5)))
       (push point current-path))
      (t
       (push (make-isochrone :time (isochrone-time isochrone)
                              :offset (isochrone-offset isochrone)
                              :path current-path)
             isochrones)
       (setf current-path (list point))))))



(defun extract-points (isochrone)
  (let ((points (loop
                   :for i :below (length isochrone) :by 3
                   :for p = (aref isochrone i)
                   :when p :collect p)))
    (make-array (length points) :initial-contents points)))

(defun extract-tracks (start-pos course-angle isochrone)
  (loop
     :for point :across isochrone
     :for k :from 0
     :when (and point ; Don't send NULL entries
                (<= (abs (- course-angle
                            (course-angle start-pos (routepoint-position point))))
                    30d0))
     :collect (do ((p point (routepoint-predecessor p))
                   (v (list)))
                  ((null p)
                   v)
                (push (routepoint-position p) v))))

(defun construct-route (isochrone dest-pos)
  (let ((best (best-point isochrone dest-pos))
        (route nil))
    (do ((cur-point best (routepoint-predecessor cur-point))
         (successor nil cur-point))
        ((null cur-point)
         route)
      (when (or (null successor)
                (routepoint-penalty cur-point)
                (not (eql (routepoint-twa cur-point) (routepoint-twa successor)))
                (not (eql (routepoint-sail cur-point) (routepoint-sail successor))))
        (push (create-trackpoint cur-point (or successor cur-point)) route)))))

(defun best-point (isochrone dest-pos)
  (loop
     :with min-dtf = nil :and min-point = nil
     :for point :across (extract-points isochrone)
     :do (progn
           (setf (routepoint-destination-distance point)
                 (fast-course-distance (routepoint-position point) dest-pos))
           (when (or (null min-point)
                     (< (routepoint-destination-distance point) min-dtf))
             (setf min-dtf (routepoint-destination-distance point)
                   min-point point)))
     :finally (return min-point)))

(defun get-statistics (track)
  (let ((sails nil)
        (min-wind 100)
        (max-wind 0)
        (min-twa 180)
        (max-twa 0))
    (dolist (point track)
      (when (trackpoint-sail point)
        (pushnew (trackpoint-sail point) sails))
      (setf min-wind
            (min (m/s-to-knots
                  (or (trackpoint-tws point) 100))
                 min-wind))
      (setf max-wind
            (max (m/s-to-knots
                  (or (trackpoint-tws point) 0)) max-wind))
      (setf min-twa
            (min (abs (or (trackpoint-twa point) 180)) min-twa))
      (setf max-twa
            (max (abs (or (trackpoint-twa point) 0)) max-twa)))
    (make-routestats :start (trackpoint-time (first track))
                     :duration (encode-duration
                                (timestamp-difference (trackpoint-time (car (last track)))
                                                      (trackpoint-time (first track))))
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


(defun get-twa-path (routing &key base-time time lat-a lng-a lat lng
                               (total-time +12h+)
                               (step-num (truncate total-time +10min+)))
  (let* ((options (encode-options (routing-options routing)))
         (polars (get-combined-polars (routing-polars routing) options))
         (time (or time (now)))
         (time-increment +10min+)
         (first-increment
          (let* ((utime (timestamp-to-unix time)))
            (- (* 600 (truncate (+ utime 600) 600))
               utime)))
         (startpos (make-latlng :latr% (rad lat-a) :lngr% (rad lng-a)))
         (targetpos (make-latlng :latr% (rad lat) :lngr% (rad lng)))
         (heading (course-angle startpos targetpos))
         (curpos-twa (copy-latlng startpos))
         (curpos-hdg (copy-latlng startpos))
         (wind-dir (interpolated-prediction (latlng-lat startpos) (latlng-lng startpos) (interpolation-parameters time base-time)))
         (twa (coerce (round (heading-twa wind-dir heading)) 'double-float))
         (twa-path nil)
         (hdg-path nil))
    (dotimes (k
              step-num
              (make-twainfo :twa twa
                            :heading (normalize-heading heading)
                            :twapath (reverse (push (list time (copy-latlng curpos-twa)) twa-path))
                            :hdgpath (reverse (push (list time (copy-latlng curpos-hdg)) hdg-path))))
      ;; Save previous position
      (push (list time (copy-latlng curpos-twa)) twa-path)
      (push (list time (copy-latlng curpos-hdg)) hdg-path)
      ;; Create new timestamp, Increment time
      (setf time (adjust-timestamp time (:offset :sec (if (= k 0) first-increment time-increment))))
      ;; Determine next position
      (multiple-value-bind (wind-dir wind-speed)
          (interpolated-prediction (latlng-lat curpos-twa) (latlng-lng curpos-twa) (interpolation-parameters time base-time))
        (declare (double-float wind-dir wind-speed))
        (multiple-value-bind (speed)
            (get-penalized-avg-speed routing nil nil wind-dir wind-speed polars twa)
          (declare (double-float speed))
          (let ((twa-heading (twa-heading wind-dir twa)))
            (setf curpos-twa
                  (add-distance-exact curpos-twa (* speed  (if (= k 0) first-increment time-increment)) twa-heading)))))
      (multiple-value-bind (wind-dir wind-speed)
          (interpolated-prediction (latlng-lat curpos-hdg) (latlng-lng curpos-hdg) (interpolation-parameters time base-time))
        (declare (double-float wind-dir wind-speed))
        (let ((heading-twa (heading-twa wind-dir heading)))
          (multiple-value-bind (speed)
              (get-penalized-avg-speed routing nil nil wind-dir wind-speed polars heading-twa)
            (setf curpos-hdg
                  (add-distance-exact curpos-hdg (* speed  (if (= k 0) first-increment time-increment)) heading))))))))


(defvar *boat-speed-ht* (make-hash-table :test #'equal))

(defun heading-twa (wind-dir heading)
  "Compute TWA resulting from HEADING in WIND"
  (normalize-angle (- wind-dir heading)))

(defun twa-boatspeed (polars wind-speed angle)
  (check-type angle angle)
  (destructuring-bind (speed sail)
      (get-max-speed (cpolars-speed polars) angle wind-speed)
    (values speed
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
