;;; Shared-clock search through gates. Crossing changes the search stage rather
;;; than ending a leg. Earlier and later crossings compete at equal timestamps.
(in-package :bitsailor)

(defparameter *gate-bucket-width* 0.24d0
  "Minimum bucket width in the stage's transformed bearing coordinates.")
(defparameter *gate-origin-distance-factor* 2d0
  "Artificial origin distance in gate widths; values below two are clamped to two.")
(defvar *gate-bucket-frames* nil
  "Request-local stage bearing transforms, independent of each actual crossing origin.")
(defstruct gate-bucket-frame axis scale)
(defparameter *gate-coverage-bins* 12
  "Number of intervals used to track valid retained crossings along each gate.")
(defparameter *gate-retirement-coverage* 1/2
  "Minimum fraction of gate intervals hit before retiring the preceding stage.
Rounded up to whole intervals: the default requires 6 of 12 (50%). This heuristic
tolerates sampling/pruning gaps but can discard useful later crossings there.
Bind to 1 to require complete coverage.")
(defvar *gate-progress* nil "Request-local radial high-water marks by stage and bucket.")
(defstruct gate-state point (stage 0) origin source crossings
  ;; Set during bucket retention; points and stage centres stay fixed thereafter.
  radius)

;;; Retire a gate stage once the configured fraction of distinct intervals has been hit.
;;; The final destination stage cannot retire; repeated hits do not increase coverage.
(defun gate-stage-retired-p (stage coverage)
  (and (< stage (length coverage))
       (let ((bins (aref coverage stage)))
         (>= (count 1 bins)
             (ceiling (* *gate-retirement-coverage* (length bins)))))))

;;; Mark along-gate intervals reached by crossings in valid retained states; repeated hits do not
;;; fill other intervals.
(defun record-gate-coverage (states gates coverage)
  (dolist (state states)
    (dolist (crossing (gate-state-crossings state))
      (let* ((stage (car crossing))
             (bins (aref coverage stage))
             (index (min (1- (length bins))
                         (floor (* (length bins)
                                   (gate-position-fraction (aref gates stage) (cdr crossing)))))))
        (setf (aref bins index) 1)))))

;;; Update request-local maximum radii for accepted states in each stage/bearing bucket.
(defun record-gate-progress (states centers)
  (when *gate-progress*
    (dolist (state states)
      (let ((key (gate-state-key state nil centers))
            (distance (course-distance (gate-stage-center state centers)
                                       (routepoint-position (gate-state-point state)))))
        (setf (gethash key *gate-progress*)
              (max distance (gethash key *gate-progress* 0d0)))))))

(defun group-gate-targets (routing)
  "Group consecutive gates with the point waypoint or destination after them."
  (let ((pending nil) (groups nil))
    (dolist (target (append (routing-waypoints routing) (list (routing-dest routing))))
      (push target pending)
      (unless (gate-p target)
        (push (nreverse pending) groups)
        (setf pending nil)))
    (nreverse groups)))

(defun gate-position-fraction (gate pos)
  "Clamped local projection of POS along GATE, with antimeridian handling."
  (let* ((a (gate-a gate)) (b (gate-b gate))
         (k (cos (rad (latlng-lat a))))
         (x (* k (normalize-angle (- (latlng-lng b) (latlng-lng a)))))
         (y (- (latlng-lat b) (latlng-lat a)))
         (px (* k (normalize-angle (- (latlng-lng pos) (latlng-lng a)))))
         (py (- (latlng-lat pos) (latlng-lat a)))
         (den (+ (* x x) (* y y))))
    (if (< den 1d-12) 0d0
        (max 0d0 (min 1d0 (/ (+ (* px x) (* py y)) den))))))

;;; Measure distance in metres to a point target or to the gate's locally projected nearest
;;; position.
(defun gate-stage-distance (target pos)
  (course-distance pos
                   (if (gate-p target)
                       (let ((a (gate-a target)) (b (gate-b target)))
                         (add-distance-exact a
                           (* (gate-position-fraction target pos) (course-distance a b))
                           (course-angle a b)))
                       target)))

;;; Return a bearing in degrees, using zero when points are less than a millimetre apart to avoid
;;; an undefined direction.
(defun gate-course-angle (from to)
  (if (< (course-distance from to) 1d-3) 0d0 (course-angle from to)))

;;; Choose bucket width from the configured minimum and per-stage point budget.
(defun gate-angular-width ()
  (max *gate-bucket-width*
       (/ 360d0 (max 1 *max-iso-points*))))

;;; Place a common origin behind the gate, away from the next target. A one-kilometre
;;; floor handles short/degenerate gates; the gate-width multiplier is at least two.
;;; Return the origin and a bearing transform which preserves lateral gate resolution.
(defun gate-artificial-origin (gate target)
  (let* ((midpoint (target-position gate))
         (width (course-distance (gate-a gate) (gate-b gate)))
         (distance (max 1000d0 (* (max 2d0 *gate-origin-distance-factor*) width)))
         (heading (gate-course-angle midpoint (target-position target)))
         (origin (add-distance-exact midpoint distance (mod (+ heading 180d0) 360d0))))
    (values origin
            (make-gate-bucket-frame
             :axis (gate-course-angle origin midpoint)
             :scale (max 1d0 (/ (* 2d0 distance) (max width 1000d0)))))))

;;; Build one fixed common origin and bearing transform per stage; stage zero uses
;;; the real start with the ordinary untransformed full-circle buckets.
(defun gate-stage-geometry (start targets)
  (let* ((count (length targets))
         (centers (make-array count))
         (frames (make-array count :initial-element nil)))
    (setf (aref centers 0) start)
    (loop :for stage :from 1 :below count :do
      (multiple-value-bind (origin frame)
          (gate-artificial-origin (aref targets (1- stage)) (aref targets stage))
        (setf (aref centers stage) origin (aref frames stage) frame)))
    (values centers frames)))

;;; Expand bearing resolution near the forward axis without adding buckets. The
;;; monotone full-circle transform is atan2(scale*sin(angle-axis), cos(angle-axis)).
;;; Only pruning/display uses it; steering and the fan retain geographic bearings.
(defun gate-bucket-angle (state centers)
  (let* ((angle (gate-course-angle (gate-stage-center state centers)
                                  (routepoint-position (gate-state-point state))))
         (frame (when *gate-bucket-frames*
                  (aref *gate-bucket-frames* (gate-state-stage state)))))
    (if frame
        (let ((relative (rad (normalize-angle (- angle (gate-bucket-frame-axis frame))))))
          (mod (deg (atan (* (gate-bucket-frame-scale frame) (sin relative))
                          (cos relative))) 360d0))
        (mod angle 360d0))))

;;; Return the fixed stage origin when available, otherwise the state's actual crossing origin.
(defun gate-stage-center (state centers)
  (if centers (aref centers (gate-state-stage state))
      (routepoint-position (gate-state-origin state))))

;;; Identify the stage and wrapped origin-bearing bucket; TARGETS is retained as an unused
;;; interface argument.
(defun gate-state-key (state targets &optional centers)
  (declare (ignore targets))
  (list (gate-state-stage state)
        (floor (gate-bucket-angle state centers)
               (gate-angular-width))))

(defun retain-gate-state (state buckets targets &optional centers)
  "Keep one outward winner per stage/bearing bucket; prefer the new state on ties.
Reject historical radial regressions. Land checks follow winner selection, as in
single-leg routing; a blocked winner leaves its bucket empty for this step."
  (let* ((key (gate-state-key state targets centers))
         (old (gethash key buckets))
         (distance (course-distance (gate-stage-center state centers)
                                    (routepoint-position (gate-state-point state)))))
    (when (and (or (null *gate-progress*)
                   (> distance (gethash key *gate-progress* -1d0)))
               (or (null old) (>= distance (gate-state-radius old))))
      (setf (gate-state-radius state) distance
            (gethash key buckets) state))))

;;; Check every actual segment added since SOURCE, including fractional gate touches and turns.
;;; A chord from SOURCE to the endpoint would miss land along a bent crossing step.
(defun gate-state-blocked-p (state)
  (loop :for point = (gate-state-point state) :then previous
        :for previous = (routepoint-predecessor point)
        :until (or (eq point (gate-state-source state)) (null previous))
        :thereis (approach-blocked-p (routepoint-position previous)
                                    (routepoint-position point))))

;;; Land-check each bucket's sole winner over the complete step, including pre-crossing travel.
;;; Discard blocked winners without a fallback; history and coverage use only valid survivors.
(defun select-gate-survivors (buckets)
  (loop :for state :being :the :hash-values :of buckets
        :unless (gate-state-blocked-p state)
          :collect state))

(defun gate-frontier-isochrones (frontier centers time wind max-step-distance &optional model wind-time)
  "Build display fragments from each stage's outer envelope, splitting gaps and radial jumps."
  (let ((result nil))
    (dotimes (stage (length centers))
      (let* ((center (aref centers stage))
             (outer (make-array (ceiling 360d0 (gate-angular-width)) :initial-element nil))
             (path nil) (previous nil) (previous-angle nil))
        (dolist (state frontier)
          (when (= stage (gate-state-stage state))
            (let* ((point (gate-state-point state))
                   (index (second (gate-state-key state nil centers)))
                   (old (aref outer index)))
              (when (or (null old)
                        (> (course-distance center (routepoint-position point))
                           (course-distance center (routepoint-position old))))
                (setf (aref outer index) point)))))
        (flet ((emit ()
                 (when path
                   (push (make-isochrone :center center :stage stage :time time
                           :forecast-model model :wind-time wind-time
                           :cycle (cl-weather:uv-cycle (params-fc1 wind))
                           :offset (truncate (timestamp-difference time
                                             (cl-weather:uv-cycle (params-fc1 wind))) 3600)
                           :path (nreverse path)) result)
                   (setf path nil))))
          (loop :for point :across outer
                :when point :do
            (let* ((pos (routepoint-position point))
                   (angle (gate-bucket-angle
                           (make-gate-state :stage stage :point point) centers)))
              (when (and previous
                         (or (> (- angle previous-angle) (max 1d0 (* 3 (gate-angular-width))))
                             (> (course-distance previous pos) (* 4 max-step-distance))))
                (emit))
              (push pos path)
              (setf previous pos previous-angle angle)))
          (emit))))
    (nreverse result)))

;;; Sail the remaining fractional step toward TARGET, choosing the direct polar angle or
;;; the better VMG-boundary tack. Reuse this step's weather sampling time, interpolate at
;;; the crossing position, and include the ordinary wave/current velocity adjustments.
;;; The endpoint stays on the shared clock; this is one heading, not a tack/gybe manoeuvre.
(defun gate-turn-endpoint (routing crossing end-time target wind current wave wave-dir)
  (let* ((pos (routepoint-position crossing))
         (lat (latlng-lat pos)) (lon (latlng-lng pos))
         (seconds (coerce (timestamp-difference end-time (routepoint-time crossing)) 'double-float))
         (bearing (gate-course-angle pos (target-position target)))
         (polars (routing-polars routing)))
    (with-bindings
        (((wind-dir wind-speed) (interpolate-uv lat lon wind))
         ((current-dir current-speed current-u current-v)
          (if current (interpolate-uv lat lon current) (values 0d0 0d0 0d0 0d0)))
         ((height direction)
          (if (and wave wave-dir)
              (values (interpolate-simple lat lon wave) (interpolate-direction lat lon wave-dir))
              (values nil nil)))
         ((up down) (best-vmg polars wind-speed)))
      (let* ((wanted (abs (heading-twa wind-dir bearing)))
             (angle (max (vmg-twa up) (min (vmg-twa down) wanted)))
             (best nil) (best-vmg nil))
        (dolist (twa (list angle (- angle)) best)
          (multiple-value-bind (speed sail) (twa-boatspeed polars wind-speed twa)
            (let* ((heading (twa-heading wind-dir twa))
                   (factor (when height (wave-factor height direction heading)))
                   (boat-speed (* speed (or factor 1d0)))
                   (sog boat-speed) (cog heading))
              (when current
                (let ((u (- (+ current-u (* boat-speed (sin (rad heading))))))
                      (v (- (+ current-v (* boat-speed (cos (rad heading)))))))
                  (setf sog (enorm u v) cog (mod (angle u v) 360d0))))
              (let ((vmg (* sog (cos (rad (normalize-angle (- cog bearing)))))))
                (when (or (null best-vmg) (> vmg best-vmg))
                  (setf best-vmg vmg
                        best (create-routepoint crossing
                               (add-distance-estimate pos (* sog seconds) cog)
                               end-time twa cog nil boat-speed sail wind-dir wind-speed
                               current-dir current-speed 0d0 0d0 height direction factor)))))))))))

(defun advance-gate-state (state point targets &optional routing wind current wave wave-dir)
  "Record ordered gate touches, with fractional timestamps on the shared-clock step.
When ROUTING is supplied and the next target is behind the incoming heading,
turn at the intersection and sail the remaining time toward that target."
  (let ((stage (gate-state-stage state))
        (origin (gate-state-origin state))
        (from (gate-state-point state))
        (crossings nil))
    (loop :while (gate-p (aref targets stage))
          :for target = (aref targets stage)
          :for fraction = (gate-crossing target (routepoint-position from)
                                         (routepoint-position point))
          :while fraction
          :do (let* ((*gate-crossings* nil)
                     (gate (make-gate :a (gate-a target) :b (gate-b target)
                                      :next (target-position (aref targets (1+ stage))))))
                (record-gate-crossing gate fraction from (routepoint-position point)
                  (timestamp-difference (routepoint-time point) (routepoint-time from))
                  (routepoint-twa point) (routepoint-heading point)
                  (routepoint-speed point) (routepoint-sail point)
                  (routepoint-wind-dir point) (routepoint-wind-speed point)
                  (routepoint-curnt-dir point) (routepoint-curnt-speed point)
                  (routepoint-wave-height point) (routepoint-wave-dir point)
                  (routepoint-wave-factor point))
                (setf from (first *gate-crossings*) origin from)
                (push (cons stage (routepoint-position from)) crossings)
                (incf stage)
                (when (and routing
                           (> (abs (normalize-angle
                                    (- (gate-course-angle (routepoint-position from)
                                         (target-position (aref targets stage)))
                                       (routepoint-heading point)))) 90d0))
                  (setf point (gate-turn-endpoint routing from (routepoint-time point)
                                (aref targets stage) wind current wave wave-dir)))))
    (setf (routepoint-predecessor point) from)
    (make-gate-state :point point :stage stage :origin origin :source (gate-state-point state) :crossings crossings)))

;;; Check the completed, possibly turned endpoint against the new actual-crossing fan.
(defun gate-state-in-fan-p (state targets fan)
  (let* ((origin (routepoint-position (gate-state-origin state)))
         (position (routepoint-position (gate-state-point state)))
         (target (target-position (aref targets (gate-state-stage state)))))
    (or (< (course-distance origin position) 1d-3)
        (<= (abs (normalize-angle (- (gate-course-angle origin position)
                                    (gate-course-angle origin target))))
            fan))))

;;; Test final arrival by proximity or by passing the destination distance within 1.5 degrees from
;;; the actual crossing origin.
(defun gate-state-reached-p (state destination)
  (let* ((point (gate-state-point state))
         (origin (routepoint-position (gate-state-origin state)))
         (pos (routepoint-position point))
         (bearing (gate-course-angle origin destination)))
    (or (< (course-distance pos destination) 1d0)
        (and (> (course-distance origin pos) 1d-3)
             (<= (abs (normalize-angle (- (gate-course-angle origin pos) bearing))) 1.5d0)
         (>= (course-distance origin pos) (course-distance origin destination))))))

;;; Select one result in a single pass, preserving the first candidate on ties.
;;; Arrivals minimize distance to the destination. Partial routes prefer the furthest
;;; stage, then minimize distance to that stage's target; no frontier reordering is needed.
(defun best-gate-state (states targets &optional arrivals-p)
  (let ((best nil) (best-stage -1) (best-distance nil))
    (dolist (state states best)
      (let ((stage (gate-state-stage state)))
        (when (or arrivals-p (>= stage best-stage))
          (let ((distance (if arrivals-p
                              (course-distance (routepoint-position (gate-state-point state))
                                               (aref targets (1- (length targets))))
                              (gate-stage-distance (aref targets stage)
                                (routepoint-position (gate-state-point state))))))
            (when (or (null best)
                      (and (not arrivals-p) (> stage best-stage))
                      (< distance best-distance))
              (setf best state best-stage stage best-distance distance))))))))

;;; Expand all active gate stages on one clock, prune radial regressions and retire covered
;;; stages; return route info, best point and last crossing origin.
(defun get-route-through-gates (routing gates)
  (declare (notinline expand-routepoint))
  (let* ((targets (coerce (append gates (list (routing-dest routing))) 'vector))
         (final-stage (length gates))
         (geometry (multiple-value-list (gate-stage-geometry (routing-start routing) targets)))
         (centers (first geometry))
         (*gate-bucket-frames* (second geometry))
         (*gate-progress* (make-hash-table :test #'equal))
         (coverage (map 'vector (lambda (gate)
                                 (declare (ignore gate))
                                 (make-array *gate-coverage-bins* :element-type 'bit :initial-element 0))
                        gates))
         (start-time (parse-datetime-local (routing-starttime routing)))
         (started (now))
         (stepper (make-stepper start-time (routing-stepmax routing) (routing-forecast-switch-time routing)))
         (initial-wind (multiple-value-list (wind-at routing (routing-start routing) start-time)))
         (initial-params (third initial-wind))
         (initial (create-routepoint nil (routing-start routing) start-time nil nil
                                    nil nil nil (first initial-wind) (second initial-wind)))
         (frontier (list (make-gate-state :point initial :origin initial)))
         (best (first frontier))
         (status "max_duration") (elapsed 0) (steps 0) (points 0) (isochrones nil))
    (loop :while (< elapsed (routing-stepmax routing))
          :do (let* ((dt (min (funcall stepper) (- (routing-stepmax routing) elapsed)))
                     (time (adjust-timestamp start-time (:offset :sec elapsed)))
                     (end-time (adjust-timestamp time (:offset :sec dt)))
                     (wind (routing-wind-params routing time))
                     (current (when (routing-currents routing) (get-params (routing-currents routing) time)))
                     (wave (when (routing-use-waves routing) (get-params 'cl-weather:gfswave-combined time)))
                     (wave-dir (when wave (get-direction-params 'cl-weather:gfswave-combined time)))
                     (buckets (make-hash-table :test #'equal))
                     (arrivals nil)
                     (max-step-distance 100d0))
                (dolist (state frontier)
                  (let* ((leg (copy-routing routing))
                         (origin (routepoint-position (gate-state-origin state)))
                         (target (target-position (aref targets (gate-state-stage state))))
                         (*routepoint-sink*
                           (lambda (point)
                             (setf max-step-distance
                                   (max max-step-distance
                                        (course-distance (routepoint-position (gate-state-point state))
                                                         (routepoint-position point))))
                             (let ((next (advance-gate-state state point targets routing wind current wave wave-dir)))
                               ;; Entering a new stage must not seed a frontier outside
                               ;; its crossing-centred fan, even on the crossing step.
                               (when (or (null (gate-state-crossings next))
                                         (gate-state-in-fan-p next targets (routing-fan leg)))
                                 (flet ((validp ()
                                          (not (gate-state-blocked-p next))))
                                   (if (and (= (gate-state-stage next) final-stage)
                                            (gate-state-reached-p next (routing-dest routing)))
                                       (when (validp) (push next arrivals))
                                       (unless (gate-stage-retired-p (gate-state-stage next) coverage)
                                         (retain-gate-state next buckets targets centers)))))))))
                    (setf (routing-start leg) origin (routing-dest leg) target
                          (routing-gate leg) nil
                          ;; Steering/fan geometry follows the actual crossing origin.
                          ;; Buckets and radial history use the common artificial
                          ;; origin. It is not a sailed position or a steering origin.
                          (routing-box leg) (make-routing-box origin target))
                    (let ((bearing (gate-course-angle origin target)))
                      (incf points (expand-routepoint leg (gate-state-point state)
                                     (mod (- bearing (routing-fan leg)) 360d0)
                                     (mod (+ bearing (routing-fan leg)) 360d0)
                                     dt end-time wind current wave wave-dir
                                     (routing-polars leg) (/ 360d0 *max-iso-points*))))))
                (incf steps) (incf elapsed dt)
                (when arrivals
                  (setf best (best-gate-state arrivals targets t)
                        status "reached")
                  (return))
                (let ((next (select-gate-survivors buckets)))
                  (record-gate-progress next centers)
                  (record-gate-coverage next targets coverage)
                  ;; Retire at the step boundary, after all simultaneous crossings
                  ;; have entered the next stage. Never resurrect a retired stage.
                  (setf next (remove-if (lambda (state)
                                         (gate-stage-retired-p (gate-state-stage state) coverage)) next))
                  (unless next (setf status "no_route") (return))
                  (setf frontier next
                        best (best-gate-state next targets)))
                (when (zerop (timestamp-minute end-time))
                  (setf isochrones
                        (nconc isochrones
                               (gate-frontier-isochrones frontier centers end-time wind
                                 max-step-distance (routing-model-at routing time) time))))))
    (let* ((rp (gate-state-point best)) (track (route-from-point rp)))
      (values (make-routeinfo :status status :best track
                :forecast-model (routing-forecast-model routing)
                :requested-cycle (when (routing-cycle routing) (cl-weather::cycle-timestamp (routing-cycle routing)))
                :initial-cycle (cl-weather:uv-cycle (params-fc1 initial-params))
                :polars (cpolars-label (routing-polars routing)) :options (routing-options routing)
                :stats (get-statistics track (timestamp-difference (now) started) steps points)
                :isochrones isochrones)
              rp (when (> (gate-state-stage best) 0) (gate-state-origin best))))))
