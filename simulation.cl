;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2025-11-04 00:50:02>

;; -- marks
;; -- atan/acos may return #C() => see CLTL
;; -- use CIS ?

(in-package :bitsailor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defparameter *sailnames* #("Jib" "Spi" "Stay" "LJ" "C0" "HG" "LG"))

(declaim (notinline twa-boatspeed))
(defun-t twa-boatspeed double-float ((polars cpolars) (wind-speed double-float) (twa double-float))
  (destructuring-bind (speed sail)
      (get-max-speed polars (abs twa) wind-speed)
    (values speed
            sail)))
;; (declaim (notinline twa-boatspeed))

(declaim (inline twa-sailspeed))
(defun twa-sailspeed (polars sail tws twa)
  (if (null sail)
      0d0
      (get-sail-speed polars (abs twa) tws sail)))

(declaim (inline twa-heading))
(defun twa-heading (wind-dir twa)
  "Compute HEADING resulting from TWA in WIND"
  (declare (double-float wind-dir twa)
           (inline normalize-heading))
  (normalize-heading (+ twa wind-dir)))
;; (declaim (notinline twa-heading))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boat speed

(declaim (inline is-tack))
(defun is-tack (a b)
  (when (< a 0) (incf a 360))
  (when (< b 0) (incf b 360)) 
  (>= (abs (- b a)) 180))

(declaim (inline get-origin-angle))
(defun get-origin-angle (start-pos new-pos origin-distance)
  (let* ((course-angle  (course-angle-d start-pos new-pos origin-distance))
         (angle (normalize-heading course-angle)))
    angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-route

(declaim (inline valid-twa))
(defun-t valid-twa boolean ((up double-float) (down double-float) (twa double-float))
  (or (not *valid-vmg-angles*)
      (and (> twa 0) (<= up twa down))))

(declaim (inline expand-routepoint))
(defun expand-routepoint (routing routepoint left right step-size step-time params polars delta-angle)
  (declare (special next-isochrone))
  (cond
    ((null routepoint)
     (return-from expand-routepoint 0))
    ;; Exclude pole region where our distance computation breaks down due to numerical instability
    ((> (latlng-lat (routepoint-position routepoint)) 89.9d0)
     (return-from expand-routepoint 0))
    (t
     (let* ((start-pos (routing-start routing))
            (lat (latlng-lat (routepoint-position routepoint)))
            (lng (latlng-lng (routepoint-position routepoint))))
       (multiple-value-bind
             (wind-dir wind-speed)
           (interpolate lat lng params)
         (setf wind-speed (max (routing-minwind routing) wind-speed))
         (multiple-value-bind (up-vmg down-vmg)
             (best-vmg polars wind-speed)
           (let ((vmg-pos (length (routing-twa-angles routing))))
             (setf (aref (routing-twa-angles routing) (- vmg-pos 1)) (vmg-twa up-vmg))
             (setf (aref (routing-twa-angles routing) (- vmg-pos 2)) (vmg-twa down-vmg)))
           (loop
             :with twa-points = (routing-twa-angles routing)
             :with up-vmg-angle = (vmg-twa up-vmg)
             :with down-vmg-angle = (vmg-twa down-vmg)
             :with added = 0
             :for twa :across twa-points
             :when (valid-twa up-vmg-angle down-vmg-angle twa)
               :do (flet ((add-point (twa)
                            (log2:trace "checking TWA: ~a" twa)
                            (progn
                              (incf added)
                              (multiple-value-bind (speed sail)
                                  (twa-boatspeed polars wind-speed (normalize-angle twa))
                                (declare (double-float speed)
                                         (integer step-size))
                                (let*
                                    ((heading (twa-heading wind-dir twa))
                                     (distance (* speed step-size))
                                     (new-pos (add-distance-exact (routepoint-position routepoint)
                                                                  distance
                                                                  heading)))
                                  (when (in-latlng-box (routing-box routing) new-pos)
                                    (let* ((origin-distance (course-distance start-pos new-pos))
                                           (origin-angle (get-origin-angle start-pos new-pos origin-distance)))
                                      (when (and (heading-between left right origin-angle))
                                                  
                                        (add-routepoint routepoint new-pos origin-distance origin-angle delta-angle left 0d0 step-size step-time twa heading speed sail wind-dir wind-speed)))))))))
                     (add-point (- twa))
                     (add-point twa))
             :finally (return added))))))))

(defstruct box north south west east antimed-p)

(defun make-routing-box (start dest &key (lat-margin 10d0) (lon-margin 10d0))
  (let* ((lat-min (min (latlng-lat start) (latlng-lat dest)))
         (lat-max (max (latlng-lat start) (latlng-lat dest)))
         (lon-min (min (latlng-lng start) (latlng-lng dest)))
         (lon-max (max (latlng-lng start) (latlng-lng dest)))
         (antimed-p (antimed-p lon-max lon-min)))
    (make-box :north (min 90 (+ lat-max lat-margin))
              :south (max -90 (- lat-min lat-margin))
              :west (if antimed-p (+ lon-min lon-margin) (- lon-min lon-margin))
              :east (if antimed-p (- lon-max lon-margin) (+ lon-max lon-margin))
              :antimed-p antimed-p)))

(defun antimed-p (lon0 lon1)
  (> (abs (- lon0 lon1)) 180d0))

(defun in-latlng-box (box pos)
  (and (<= (box-south box) (latlng-lat pos) (box-north box))
       (if (box-antimed-p box)
           (or
            (longitude-between (box-west box) (latlng-lng pos) (box-east box)))
           (longitude-between (box-east box) (latlng-lng pos) (box-west box)))))

(defun get-route (routing)
  (bordeaux-threads:with-lock-held (+last-request-lock+)
    (setf *last-request*
          (push (list (local-time:now) 'get-route) *last-request*)))
  (let* ((start-pos (routing-start routing))
         (destination (normalized-destination routing))
         (distance (course-distance start-pos destination))
         (polars (routing-polars routing))
         (race-info (routing-race-info routing))
         (limits (when (race-info-vr-p race-info)
                   (get-race-limits race-info)))
         (zones (when (race-info-vr-p race-info)
                  (get-exclusion-zones race-info)))
         (dest-heading (normalize-heading (course-angle start-pos destination)))
         (left (normalize-heading (coerce (- dest-heading (routing-fan routing)) 'double-float)))
         (right (normalize-heading (coerce (+ dest-heading (routing-fan routing)) 'double-float)))
         (start-time
           (parse-datetime-local (routing-starttime routing) :timezone "+00:00"))
         (cycle (routing-cycle routing))
         (resolution (routing-resolution routing)) 
         (isochrones nil))
    (log2:info "Routing from ~a to ~a at ~a Course angle ~a Fan ~a Left ~a Right ~a Cycle ~a"
                (format-latlng nil start-pos)
                (format-latlng nil destination)
                start-time
                dest-heading
                (routing-fan routing)
                left
                right
                cycle)
    (do* ( ;; Iteration stops when destination was reached
          (reached nil)
          (error nil)
          ;; Iteration stops when stepmax seconds have elapsed
          (stepnum 1 (1+ stepnum))
          (pointnum 0)
          (elapsed0 (now))
          ;; Increase max-points per isochrone as the isochrones expand to keep resolution roughly constant
          (max-points *max-iso-points* (min *max-iso-points* (+ max-points 10)))
          (delta-angle (/ 360d0 max-points) (/ 360d0 max-points))
          (max-dist (make-array *max-iso-points* :initial-element 0d0))
          (min-angle (/ 360d0 *max-iso-points*))
          ;; The first step-size and when we apply it is important - brings step-time to mod 10min
          (stepper (make-stepper start-time  (routing-stepmax routing)))
          ;; Get wind data for simulation time
          ;; Advance the simulation time AFTER each iteration - this is most likely what GE does
          (params (interpolation-parameters start-time
                                            :source (routing-grib-source routing)
                                            :method (routing-interpolation routing)
                                            :merge-start (routing-merge-start routing)
                                            :merge-duration (routing-merge-window routing)
                                            :cycle cycle
                                            :resolution resolution)
                  (interpolation-parameters step-time
                                            :source (routing-grib-source routing)
                                            :method (routing-interpolation routing)
                                            :merge-start (routing-merge-start routing)
                                            :merge-duration (routing-merge-window routing)
                                            :cycle cycle
                                            :resolution resolution))
          (base-time (iparams-effective-cycle params)
                     (iparams-effective-cycle params))
          (step-size (funcall stepper)
                     (funcall stepper))
          (step-time (adjust-timestamp start-time (:offset :sec step-size))
                     (adjust-timestamp step-time (:offset :sec step-size)))
          (stepsum 0 (+ stepsum step-size))
          ;; The initial isochrone is just the start point, heading towards destination
          (isochrone
           (multiple-value-bind (wind-dir wind-speed) 
               (interpolate (latlng-lat start-pos) (latlng-lng start-pos) params)
             (let ((sail (position (routing-sail routing) *sailnames* :test #'string=)))
               (make-array 1 :initial-contents
                           (list
                            (create-routepoint nil start-pos start-time (routing-tack routing) nil (course-distance start-pos destination) nil sail wind-dir wind-speed))))))
          ;; The next isochrone - in addition we collect all hourly isochrones
          (next-isochrone (make-array max-points :initial-element nil)
                          (make-array max-points :initial-element nil)))
         ;; When the maximum number of iterations is reached, construct the best path
         ;; from the most advanced point's predecessor chain.
         ((or reached
              error
              (>= stepsum (routing-stepmax routing)))
          (let ((elapsed (timestamp-difference (now) elapsed0)))
            (multiple-value-bind  (best-route best-path)
                (construct-route routing isochrone destination)
              (log-stats elapsed stepnum pointnum)
              (make-routeinfo :status (if reached "reached"
                                          (if error "no_route"
                                              "max_duration"))
                              :best best-route
                              :path best-path
                              :stats (get-statistics best-route elapsed stepnum pointnum)
                              :polars (cpolars-label polars)
                              :options (routing-options routing)
                              :maxspeed "-1"
                              :tracks (when *tracks*
                                        (extract-tracks start-pos (course-angle start-pos destination) isochrone))
                              :isochrones (prepare-routepoints isochrones)))))
      
      (declare (fixnum max-points step-size stepsum pointnum)
               (special next-isochrone))
      (declare (vector next-isochrone))
      (declare (special max-dist min-angle))

      (log2:trace "Step ~a, Isochrone max points: ~a" step-size max-points)
      
      ;; Step 1 - Compute next isochrone by exploring from each point in the current isochrone.
      ;;          Add new points to next-isochrone.
      (map nil (lambda (rp)
                 (let ((new-point-num
                         (expand-routepoint routing rp left right step-size step-time params polars delta-angle)))
                   (declare (fixnum new-point-num))
                   (incf pointnum new-point-num)))
           isochrone)
      
      ;; Step 2 - Filter isochrone.
      (let ((candidate (filter-isochrone next-isochrone :limits limits :zones zones)))
        (cond
          ((or (null candidate)
               (= (length candidate) 0)
               (notany #'routepoint-p candidate))
           (log2:warning "Routing aborted: Candidate: ~a, Length: ~A, Routepoints: ~a"
                         (not (null candidate))
                         (length candidate)
                         (notany #'routepoint-p candidate))
           (setf error t))
          (t
           (setf reached (reached candidate start-pos dest-heading distance))
           (setf isochrone candidate)

           (log2:trace-more "Isochrone ~a at ~a, ~a points" stepnum (format-datetime nil step-time) (length isochrone))

           ;; Collect hourly isochrones
           (when (zerop (timestamp-minute step-time))
             (let* ((iso (make-isochrone :center start-pos
                                         :time base-time
                                         :offset (truncate (timestamp-difference step-time base-time) 3600)
                                         :path (extract-points isochrone))))
               (push iso isochrones)))))
        (when reached
          (log2:trace "Reached destination at ~a" step-time))))))

(defun reached (candidate start angle distance)
  (some (lambda (p)
          (let ((a  (routepoint-origin-angle p))
                (d  (routepoint-origin-distance p)))
            (and p
                 (<= (abs (- angle a)) 1.5d0)
                 (>= d distance))))
        candidate))

(defun get-exclusion-zones (leg-info)
  (let* ((zones (joref (race-info-data leg-info) "restrictedZones"))
         (result (make-array (length zones))))
    (map-into result
              (lambda (z)
                (get-exclusion-zone z))
              zones)))

(defun get-exclusion-zone (zone-def)
  (loop :with vertices = (joref zone-def "vertices")
        :with result = (make-array (1+ (length vertices)) :fill-pointer 0)
        :for vertice :across vertices
        :for latlng = (make-latlng :lat (coerce (joref vertice "lat") 'double-float)
                                   :lng (coerce (joref vertice "lon") 'double-float))
        :do (vector-push latlng result)
        :finally (progn (unless (equalp (aref result 0) (aref result (1- (length result))))
                          (vector-push  (aref result 0) result))
                        (return result))))

(defun get-race-limits (leg-info)
  (let* ((south (joref (joref (race-info-data leg-info) "ice_limits") "south"))
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

(defun make-stepper (start-time step-max)
  (let* ((step-time
           start-time)
         (start-seconds
           (timestamp-to-unix start-time))
         (start
          (* 600 (ceiling start-seconds 600)))
         (hour-fill
           (- 3600 (mod start 3600)))
         (hour-fill-2
           (if (<= step-max 21600)
               0
               hour-fill)))
    (lambda ()
      (let ((step-size
              (cond
                ((eql step-time start-time)
                 ;; First step - runs up to full 10min
                 (- 600 (mod start-seconds 600)))
                (t
                 (let ((delta-t
                         (- (timestamp-to-unix step-time) start)))
                   (cond ((and (<= step-max 21600) ;; 6*3600s
                               (< delta-t (+ hour-fill (* 120 60))))
                          120)
                         ((< delta-t (+ hour-fill-2 (* 36 600)))
                          600)
                         ((< delta-t (+ hour-fill-2 (* 144 600)))
                          900)
                         ((< delta-t (+ hour-fill-2 (* 288 600)))
                          1800)
                         (t
                          3600)))))))
        (setf step-time (adjust-timestamp step-time (:offset :sec step-size)))
        (values step-size
                step-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Speed

(declaim (inline foiling-factor))
(defun-t foiling-factor double-float ((speed double-float) (twa double-float))
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

(defun log-stats (elapsed stepnum pointnum)
  (log2:trace "Elapsed ~2$, Positions ~a, Isochrones ~a | p/s=~2$ | s/i=~4$ | p/i=~2$"
              elapsed
              pointnum
              stepnum
              (/ pointnum elapsed)
              (/ elapsed stepnum)
              (coerce (/ pointnum stepnum) 'float)))

(defun normalized-destination (routing)
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
                              :path (loop
                                       :for r :in (isochrone-path i)
                                       :collect (routepoint-position r)))))

(defun construct-isochrones (isochrones)
  (loop
    :for isochrone :in isochrones
    :for i = (separate-isochrones isochrone)
    :append i))

(defun separate-isochrones (isochrone)
  (do* ((path (isochrone-path isochrone))
        (index 0 (1+ index))
        (previous nil point)
        (point (aref path index) (aref path index))
        (current-path nil)
        (isochrones nil))
       ((>= index (1- (length path)))
        (cons (make-isochrone :center (isochrone-center isochrone)
                              :time (isochrone-time isochrone)
                              :offset (isochrone-offset isochrone)
                              :path current-path)
              isochrones))
      (cond
        ((or (null previous)
             (let ((angle (abs (- (routepoint-origin-angle point)
                                  (routepoint-origin-angle previous)))))
               (or (< angle 1d0) (> angle 355d0))))
         (push point current-path))
        (t
         (push (make-isochrone :time (isochrone-time isochrone)
                               :offset (isochrone-offset isochrone)
                               :path current-path)
               isochrones)
         (setf current-path (list point))))))


(defun extract-points (isochrone)
  (let ((points (loop
                   :for i :below (length isochrone) :by 1
                   :for p = (aref isochrone i)
                   :when p :collect p)))
    (make-array (length points) :initial-contents points)))

(defun-t arc-length double-float ((alpha double-float)  (beta double-float))
  (when (< alpha 0) (incf alpha 360d0))
  (when (< beta 0) (incf beta 360d0))
  (when (< beta alpha) (incf beta 360d0))
  (- beta alpha))

(defun extract-tracks (start-pos course-angle isochrone)
  (loop
    :for point :across isochrone
    :for angle = (course-angle start-pos (routepoint-position point))
    :for delta = (min (arc-length course-angle angle)
                      (arc-length angle course-angle))
    :when (and point ; Don't send NULL entries
               (<= delta 30d0))
      :collect (do ((p point (routepoint-predecessor p))
                    (v (list)))
                   ((null p)
                    v)
                 (push (routepoint-position p) v))))

(declaim (inline best-point))
(defun best-point (isochrone destination)
  (loop
     :with min-dtf = nil :and min-point = nil
     :for point :across isochrone
     :do (progn
           (setf (routepoint-destination-distance point)
                 (course-distance (routepoint-position point) destination))
           (when (or (null min-point)
                     (< (routepoint-destination-distance point) min-dtf))
             (setf min-dtf (routepoint-destination-distance point)
                   min-point point)))
     :finally (return min-point)))

(defun construct-route (routing isochrone destination)
  (let  ((best (best-point isochrone destination)))
    (do ((route nil)
         (path nil)
         (cur-point best (routepoint-predecessor cur-point))
         (successor nil cur-point))
        ((null cur-point)
         (values route
                 path))
      (push (routepoint-position cur-point) path)
      (when (or (not (routing-simplify-route routing))
                (null successor)
                ;; (not (eql (routepoint-speed cur-point) (routepoint-speed successor)))
                (not (eql (routepoint-twa cur-point) (routepoint-twa successor)))
                (not (eql (routepoint-sail cur-point) (routepoint-sail successor))))
        (push (create-trackpoint cur-point (or successor cur-point)) route)))))


(defun get-statistics (track elapsed stepnum pointnum)
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
    (let ((routestats
            (make-routestats :start (trackpoint-time (first track))
                             :duration (encode-duration
                                        (timestamp-difference (trackpoint-time (car (last track)))
                                                              (trackpoint-time (first track))))
                             :sails sails
                             :min-wind min-wind
                             :max-wind max-wind
                             :min-twa min-twa
                             :max-twa max-twa
                             :calctime elapsed
                             :steps stepnum
                             :points pointnum)))
      (bt:with-lock-held (+last-routestats-lock+)
        (setf *last-routestats* routestats))
      (values routestats))))

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

(defun get-twa-path (routing &key cycle time lat-a lng-a lat lng
                               (total-time +24h+)
                               (step-num (truncate total-time +10min+)))
  (let* ((time (or time (now)))
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
         (wind-dir (interpolate (latlng-lat startpos)
                                (latlng-lng startpos)
                                (interpolation-parameters time
                                                          :gfs-mode (routing-gfs-mode routing)
                                                          :source (routing-grib-source routing)
                                                          :method (routing-interpolation routing)
                                                          :merge-start (routing-merge-start routing)
                                                          :merge-duration (routing-merge-window routing)
                                                          :resolution (routing-resolution routing)
                                                          :cycle cycle)))
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
      ;; Determine next position - TWA
      (multiple-value-bind (wind-dir wind-speed)
          (interpolate (latlng-lat curpos-twa)
                       (latlng-lng curpos-twa)
                       (interpolation-parameters time
                                                 :gfs-mode (routing-gfs-mode routing)
                                                 :source (routing-grib-source routing)
                                                 :method (routing-interpolation routing)
                                                 :merge-start (routing-merge-start routing)
                                                 :merge-duration (routing-merge-window routing)
                                                 :resolution (routing-resolution routing)
                                                 :cycle cycle))
        (declare (double-float wind-dir wind-speed))
        (multiple-value-bind (speed)
            (twa-boatspeed (routing-polars routing) wind-speed twa)
          (declare (double-float speed))
          (let ((twa-heading (twa-heading wind-dir twa)))
            (setf curpos-twa
                  (add-distance-exact curpos-twa (* speed  (if (= k 0) first-increment time-increment)) twa-heading)))))
      ;; Determine next position - HDG
      (multiple-value-bind (wind-dir wind-speed)
          (interpolate (latlng-lat curpos-hdg)
                       (latlng-lng curpos-hdg)
                       (interpolation-parameters time
                                                 :gfs-mode (routing-gfs-mode routing)
                                                 :source (routing-grib-source routing)
                                                 :method (routing-interpolation routing)
                                                 :merge-start (routing-merge-start routing)
                                                 :merge-duration (routing-merge-window routing)
                                                 :resolution (routing-resolution routing)
                                                 :cycle cycle))
        (declare (double-float wind-dir wind-speed))
        (let ((heading-twa (heading-twa wind-dir heading)))
          (multiple-value-bind (speed)
              (twa-boatspeed polars wind-speed heading-twa)
            (setf curpos-hdg
                  (add-distance-exact curpos-hdg (* speed  (if (= k 0) first-increment time-increment)) heading)))))
      (setf time (adjust-timestamp time (:offset :sec (if (= k 0) first-increment time-increment)))))))


(defvar *boat-speed-ht* (make-hash-table :test #'equal))

(defun heading-twa (wind-dir heading)
  "Compute TWA resulting from HEADING in WIND"
  (normalize-angle (- heading wind-dir)))

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
