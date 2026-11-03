;;; Constrained TWA shooting after the isochrone search. Controls are signed TWA
;;; and elapsed seconds; geographic headings are derived from the evolving wind.
(in-package :bitsailor)

(defparameter *fine-tune-enabled* t "Apply bounded TWA post-processing to completed routes.")
(defparameter *fine-tune-step* 120d0 "Maximum integration step in seconds for trial TWA segments.")
(defparameter *fine-tune-window* 3600d0 "Maximum ordinary shooting-window duration in seconds.")
(defparameter *fine-tune-angle-limit* 5d0 "Maximum TWA departure from a window's original angle range.")
(defparameter *fine-tune-position-limit* 500d0 "Maximum corridor and tack/gybe displacement in metres.")
(defparameter *fine-tune-time-limit* 300d0 "Maximum tack/gybe time displacement in seconds.")
(defparameter *fine-tune-endpoint-tolerance* 0.5d0 "Numerical endpoint closure tolerance in metres.")
(defparameter *fine-tune-max-evaluations* 3000 "Maximum trial integrations per route.")
(defparameter *fine-tune-max-seconds* 10d0 "Wall-clock trial budget; checked between integrations.")
(defvar *fine-tune-evaluations* 0)
(defvar *fine-tune-started* 0)
(defvar *fine-tune-weather* nil)

(defstruct twa-segment start-time end-time twa)
(defstruct fine-tuning status reason evaluations original-changes optimized-changes
  original-arrival optimized-arrival max-manoeuvre-distance max-manoeuvre-seconds)
(defstruct fine-manoeuvre time position kind)

;;; Convert seconds, including fractions, without rounding manoeuvres to routing steps.
(defun fine-time+ (time seconds)
  (adjust-timestamp time (:offset :nsec (round (* seconds 1000000000)))))

;;; Local endpoint residual in east/north metres; unlike acos distance this is stable at closure.
(defun fine-offset (from to)
  (values (* 6371000d0 (cos (/ (+ (latlng-latr from) (latlng-latr to)) 2d0))
             (rad (normalize-angle (- (latlng-lng to) (latlng-lng from)))))
          (* 6371000d0 (- (latlng-latr to) (latlng-latr from)))))

;;; Euclidean local distance for numerical closure, including antimeridian wrapping.
(defun fine-distance (a b)
  (multiple-value-bind (x y) (fine-offset a b) (sqrt (+ (* x x) (* y y)))))

;;; Count genuine control changes rather than interpolation samples or sail changes.
(defun fine-changes (track)
  (loop for (a b c) on track while c
        count (> (abs (- (trackpoint-twa a) (trackpoint-twa b))) 0.05d0)))

;;; Export a compact, executable signed-TWA plan. Equal consecutive angles are one segment.
(defun fine-segments (track)
  (let ((result nil))
    (loop for (a b) on track while b
          for angle = (trackpoint-twa a)
          when (and (numberp angle) (timestamp< (trackpoint-time a) (trackpoint-time b))) do
      (if (and result (< (abs (- angle (twa-segment-twa (first result)))) 1d-7))
          (setf (twa-segment-end-time (first result)) (trackpoint-time b))
          (push (make-twa-segment :start-time (trackpoint-time a)
                  :end-time (trackpoint-time b) :twa angle) result)))
    (nreverse result)))

;;; Record the original manoeuvre order and type; signed-angle flips distinguish the tacks.
(defun fine-manoeuvres (track)
  (loop for (a b c) on track while c
        when (minusp (* (trackpoint-twa a) (trackpoint-twa b)))
          collect (make-fine-manoeuvre :time (trackpoint-time b) :position (trackpoint-position b)
                    :kind (if (< (+ (abs (trackpoint-twa a)) (abs (trackpoint-twa b))) 180d0)
                              :tack :gybe))))

;;; Compare manoeuvres in order, returning validity and the largest position/time changes.
(defun fine-check-manoeuvres (reference candidate)
  (let ((a (fine-manoeuvres reference)) (b (fine-manoeuvres candidate)) (space 0d0) (seconds 0d0))
    (unless (= (length a) (length b)) (return-from fine-check-manoeuvres nil))
    (loop for old in a for new in b do
      (unless (eq (fine-manoeuvre-kind old) (fine-manoeuvre-kind new))
        (return-from fine-check-manoeuvres nil))
      (setf space (max space (fine-distance (fine-manoeuvre-position old) (fine-manoeuvre-position new)))
            seconds (max seconds (abs (timestamp-difference (fine-manoeuvre-time old) (fine-manoeuvre-time new))))))
    (values (and (<= space *fine-tune-position-limit*) (<= seconds *fine-tune-time-limit*)) space seconds)))

;;; Cache forecast parameters by exact sample time, never by candidate position or heading.
;;; Parameters still interpolate at each sailed position. Model switches remain absolute.
(defun fine-weather (routing time)
  (let ((key (list (timestamp-to-unix time) (nsec-of time))))
    (or (gethash key *fine-tune-weather*)
        (setf (gethash key *fine-tune-weather*)
              (list (routing-wind-params routing time)
                    (when (routing-currents routing) (get-params (routing-currents routing) time))
                    (when (routing-use-waves routing) (get-params 'cl-weather:gfswave-combined time))
                    (when (routing-use-waves routing) (get-direction-params 'cl-weather:gfswave-combined time)))))))

;;; Advance a signed TWA in live local wind, using the router's polar, wave and current physics.
;;; Return the outgoing sample and its endpoint. No target bearing is used to steer.
(defun fine-sail-step (routing pos time seconds twa)
  (destructuring-bind (wind current wave wave-dir) (fine-weather routing time)
    (let ((lat (latlng-lat pos)) (lon (latlng-lng pos)) (polars (routing-polars routing)))
      (with-bindings
          (((twd tws) (interpolate-uv lat lon wind))
           ((up down) (best-vmg polars tws))
           ((speed sail) (twa-boatspeed polars tws twa))
           ((cd cs cu cv) (if current (interpolate-uv lat lon current) (values 0d0 0d0 0d0 0d0)))
           ((height direction) (if (and wave wave-dir)
                                  (values (interpolate-simple lat lon wave) (interpolate-direction lat lon wave-dir))
                                  (values nil nil))))
        (unless (and (> (abs twa) 0d0) (<= (vmg-twa up) (abs twa) (vmg-twa down))
                     (> speed 0.01d0))
          (return-from fine-sail-step nil))
        (let* ((heading (twa-heading twd twa))
               (factor (when height (wave-factor height direction heading)))
               (boat-speed (* speed (or factor 1d0)))
               (sog boat-speed) (cog heading))
          (when current
            (let ((u (- (+ cu (* boat-speed (sin (rad heading))))))
                  (v (- (+ cv (* boat-speed (cos (rad heading)))))))
              (setf sog (enorm u v) cog (mod (angle u v) 360d0))))
          (values (make-trackpoint :time time :position pos :twa twa :heading cog :dtf (course-distance pos (routing-dest routing))
                    :speed boat-speed :sail sail :twd twd :tws tws :crntdir cd :crntspd cs
                    :wave-height height :wave-dir direction :wave-factor factor
                    :wind-cycle (cl-weather:uv-cycle (params-fc1 wind))
                    :wind-model (routing-model-at routing time) :wind-time time)
                  (add-distance-estimate pos (* sog seconds) cog)))))))

;;; Reject trial integrations once the bounded post-processing budget has been spent.
(defun fine-budget-p ()
  (and (< *fine-tune-evaluations* *fine-tune-max-evaluations*)
       (< (/ (- (get-internal-real-time) *fine-tune-started*) internal-time-units-per-second)
          *fine-tune-max-seconds*)))

;;; Re-sail (TWA . seconds) controls. Trial integration samples at most STEP seconds apart,
;;; exactly splits forecast-model switches, and includes all control boundaries in the track.
;;; Baseline replay can preserve the original step lengths with STEP=NIL and COUNTED=NIL.
(defun fine-replay (routing pos time controls &key (step *fine-tune-step*) (counted t))
  (when counted
    (unless (fine-budget-p) (return-from fine-replay nil))
    (incf *fine-tune-evaluations*))
  (let ((track nil) (last-sample nil))
    (dolist (control controls)
      (let ((remaining (cdr control)))
        (unless (> remaining 0d0) (return-from fine-replay nil))
        (loop while (> remaining 1d-7) do
          (let* ((switch (routing-forecast-switch-time routing))
                 (dt (min remaining (or step remaining)
                          (if (and switch (timestamp< time switch))
                              (timestamp-difference switch time) remaining))))
            (multiple-value-bind (sample endpoint) (fine-sail-step routing pos time dt (car control))
              (unless sample (return-from fine-replay nil))
              (push sample track)
              (setf last-sample sample pos endpoint time (fine-time+ time dt))
              (decf remaining dt))))))
    (when last-sample
      (let ((end (copy-trackpoint last-sample)))
        (setf (trackpoint-position end) pos (trackpoint-time end) time
              (trackpoint-dtf end) (course-distance pos (routing-dest routing)))
        (push end track)))
    (nreverse track)))

;;; Extract original controls without zero-time intersection duplicates.
(defun fine-controls (points)
  (loop for (a b) on points while b
        for dt = (timestamp-difference (trackpoint-time b) (trackpoint-time a))
        when (> dt 1d-7) collect (cons (coerce (trackpoint-twa a) 'double-float) (coerce dt 'double-float))))

;;; Position on a reference route at TIME, used only for a corridor test, never for sailing.
(defun fine-reference-position (points time)
  (loop for (a b) on points while b
        when (timestamp<= time (trackpoint-time b)) do
          (let* ((dt (timestamp-difference (trackpoint-time b) (trackpoint-time a)))
                 (f (if (> dt 0d0) (max 0d0 (min 1d0 (/ (timestamp-difference time (trackpoint-time a)) dt))) 0d0))
                 (p (trackpoint-position a)) (q (trackpoint-position b)))
            (return-from fine-reference-position
              (make-latlng :lat (+ (latlng-lat p) (* f (- (latlng-lat q) (latlng-lat p))))
                          :lng (normalize-angle (+ (latlng-lng p) (* f (normalize-angle (- (latlng-lng q) (latlng-lng p))))))))))
  (trackpoint-position (car (last points))))

;;; Validate all simulated segments and the time-aligned original-route corridor.
(defun fine-valid-window-p (reference candidate)
  (and candidate
       (<= (fine-distance (trackpoint-position (car (last reference)))
                          (trackpoint-position (car (last candidate)))) *fine-tune-endpoint-tolerance*)
       (fine-check-manoeuvres reference candidate)
       (every (lambda (p) (<= (fine-distance (trackpoint-position p)
                                              (fine-reference-position reference (trackpoint-time p)))
                               *fine-tune-position-limit*)) candidate)
       (loop for (a b) on candidate while b
             never (approach-blocked-p (trackpoint-position a) (trackpoint-position b)))))

;;; Numerically solve two endpoint constraints for two continuous control parameters.
;;; Finite differences and damped Newton steps avoid depending on a new solver library.
(defun fine-shoot (routing pos time target builder seed limits probes)
  (let ((x (copy-list seed)))
    (labels ((evaluate-at (v)
               (when (every (lambda (value range) (<= (car range) value (cdr range))) v limits)
                 (let ((track (fine-replay routing pos time (funcall builder v))))
                   (when track
                     (multiple-value-bind (east north)
                         (fine-offset target (trackpoint-position (car (last track))))
                       (list track east north (+ (* east east) (* north north)))))))))
      (loop repeat 8 do
        (let ((base (evaluate-at x)))
          (unless base (return-from fine-shoot nil))
          (when (<= (fourth base) (expt *fine-tune-endpoint-tolerance* 2))
            (return-from fine-shoot (first base)))
          (let* ((p (evaluate-at (list (+ (first x) (first probes)) (second x))))
                 (q (evaluate-at (list (first x) (+ (second x) (second probes))))))
            (unless (and p q) (return-from fine-shoot nil))
            (let* ((a (/ (- (second p) (second base)) (first probes)))
                   (b (/ (- (second q) (second base)) (second probes)))
                   (c (/ (- (third p) (third base)) (first probes)))
                   (d (/ (- (third q) (third base)) (second probes)))
                   (det (- (* a d) (* b c))))
              (when (< (abs det) 1d-10) (return-from fine-shoot nil))
              (let ((dx (/ (- (* b (third base)) (* d (second base))) det))
                    (dy (/ (- (* c (second base)) (* a (third base))) det))
                    (next nil))
                (dolist (factor '(1d0 0.5d0 0.25d0 0.125d0))
                  (let* ((v (list (+ (first x) (* factor dx)) (+ (second x) (* factor dy))))
                         (trial (evaluate-at v)))
                    (when (and trial (< (fourth trial) (fourth base))) (setf next v) (return))))
                (unless next (return-from fine-shoot nil))
                (setf x next)))))))))

;;; Bound candidate angles to the original side and local range. This cannot insert a tack.
(defun fine-angle-bounds (controls)
  (let* ((angles (mapcar #'car controls)) (positive (plusp (first angles))))
    (cons (max (if positive 0.1d0 -179.9d0) (- (reduce #'min angles) *fine-tune-angle-limit*))
          (min (if positive 179.9d0 -0.1d0) (+ (reduce #'max angles) *fine-tune-angle-limit*)))))

;;; Objective among feasible equal-time windows: fewer TWA changes, then less total variation.
;;; Manoeuvre movement is discouraged unless it reduces steering variation.
(defun fine-window-score (reference candidate)
  (+ (* 1000d0 (fine-changes candidate))
     (loop for (a b c) on candidate while c sum (abs (- (trackpoint-twa a) (trackpoint-twa b))))
     (loop for a in (fine-manoeuvres reference) for b in (fine-manoeuvres candidate)
           sum (* 0.001d0 (abs (timestamp-difference (fine-manoeuvre-time a) (fine-manoeuvre-time b)))))))

;;; Fit one/two TWA segments. A single tack/gybe may move within its hard bounds.
;;; Only the final window may finish early; all intermediate clock anchors stay fixed.
(defun fine-optimize-window (routing reference pos finalp)
  (let* ((controls (fine-controls reference))
         (time (trackpoint-time (first reference)))
         (target (trackpoint-position (car (last reference))))
         (duration (timestamp-difference (trackpoint-time (car (last reference))) time))
         (events (fine-manoeuvres reference))
         (best nil) (score most-positive-double-float))
    (when (or (null controls) (> (length events) 1)) (return-from fine-optimize-window nil))
    (labels ((consider (track)
               (when (and (fine-valid-window-p reference track)
                          (timestamp<= (trackpoint-time (car (last track))) (trackpoint-time (car (last reference)))))
                 (let ((value (fine-window-score reference track)))
                   (when (or (null best)
                             (and finalp (timestamp< (trackpoint-time (car (last track)))
                                                    (trackpoint-time (car (last best)))))
                             (and (timestamp= (trackpoint-time (car (last track))) (trackpoint-time (car (last best))))
                                  (< value score)))
                     (setf best track score value))))))
      ;; The final same-tack window can close on a single intermediate TWA and
      ;; a shorter duration. Probe duration inward, since a later ETA is forbidden.
      (when (and finalp (null events) (fine-budget-p))
        (let ((average (/ (loop for c in controls sum (* (car c) (cdr c))) duration)))
          (consider (fine-shoot routing pos time target
                      (lambda (v) (list (cons (first v) (second v))))
                      (list average (* duration 0.999d0))
                      (list (fine-angle-bounds controls) (cons (* duration 0.8d0) duration))
                      '(0.1d0 -0.5d0)))))
      (let* ((event (first events))
             (boundary (when event (timestamp-difference (fine-manoeuvre-time event) time)))
             (before (if event (loop for c in controls until (minusp (* (caar controls) (car c))) collect c) controls))
             (after (if event (nthcdr (length before) controls) controls))
             (bounds-a (fine-angle-bounds before)) (bounds-b (fine-angle-bounds after))
             (a (if event (/ (loop for c in before sum (* (car c) (cdr c))) (reduce #'+ before :key #'cdr)) (caar controls)))
             (b (if event (/ (loop for c in after sum (* (car c) (cdr c))) (reduce #'+ after :key #'cdr))
                    (if (< a (/ (loop for c in controls sum (* (car c) (cdr c))) duration))
                        (reduce #'max controls :key #'car) (reduce #'min controls :key #'car))))
             (fractions (if event
                            (loop for shift in '(0d0 -60d0 60d0 -180d0 180d0 -300d0 300d0)
                                  when (<= (abs shift) *fine-tune-time-limit*) collect (/ (+ boundary shift) duration))
                            '(0.5d0 0.25d0 0.75d0))))
        (dolist (ratio (if finalp '(1d0 0.999d0 0.995d0 0.99d0 0.98d0) '(1d0)))
          (dolist (fraction fractions)
            (when (and (fine-budget-p) (< 0.01d0 fraction 0.99d0) (> (abs (- a b)) 0.05d0))
              (consider (fine-shoot routing pos time target
                          (lambda (v) (list (cons (first v) (* duration ratio fraction))
                                            (cons (second v) (* duration ratio (- 1d0 fraction)))))
                          (list a b) (list bounds-a bounds-b) '(0.1d0 0.1d0)))))))
      best)))

;;; Split at point-waypoint anchors, duration/range limits and before a second manoeuvre.
;;; Gate compliance is checked independently over the final chronological sailed path.
(defun fine-windows (routing track)
  (let ((result nil) (window (list (first track)))
        (changes 0) (previous-angle nil) (minimum nil) (maximum nil))
    (labels ((finish (point)
               (when (rest window) (push (nreverse window) result))
               (setf window (list point) changes 0 previous-angle nil minimum nil maximum nil)))
      (loop for (a b c) on track while b do
        (let* ((angle (trackpoint-twa a))
               (flip (and previous-angle (minusp (* previous-angle angle))))
               (new-min (min (or minimum (abs angle)) (abs angle)))
               (new-max (max (or maximum (abs angle)) (abs angle))))
          ;; Split BEFORE including a large intended steering change or a second
          ;; manoeuvre; otherwise the window's advertised angle bounds would widen.
          (when (and (rest window)
                     (or (> (- new-max new-min) (* 2d0 *fine-tune-angle-limit*))
                         (and flip (plusp changes))))
            (finish a)
            (setf flip nil new-min (abs angle) new-max (abs angle)))
          (when flip (incf changes))
          (setf previous-angle angle minimum new-min maximum new-max)
          (push b window)
          (when (or (null c)
                    (>= (timestamp-difference (trackpoint-time b)
                                              (trackpoint-time (car (last window)))) *fine-tune-window*)
                    (some (lambda (w) (and (not (gate-p w))
                                          (< (fine-distance w (trackpoint-position b)) 1d0)))
                          (routing-waypoints routing)))
            (finish b)))))
    (nreverse result)))

;;; Verify ordered targets over actual segments. Touches within closure tolerance count;
;;; multiple gates in one segment must occur in the requested spatial order.
(defun fine-check-targets (routing track)
  (let ((targets (copy-list (routing-waypoints routing))))
    (loop for (a b) on track while b do
      (let ((from (trackpoint-position a)) (to (trackpoint-position b)) (fraction 0d0))
        (loop while targets do
          (let ((target (first targets)))
            (if (gate-p target)
                (let ((hit (gate-crossing target from to)))
                  (cond ((and hit (>= hit fraction)) (setf fraction hit) (pop targets))
                        ((and (<= fraction 0d0) (<= (gate-stage-distance target from) *fine-tune-endpoint-tolerance*))
                         (pop targets))
                        ((<= (gate-stage-distance target to) *fine-tune-endpoint-tolerance*)
                         (setf fraction 1d0) (pop targets))
                        (t (return))))
                (if (<= (fine-distance target to) *fine-tune-endpoint-tolerance*)
                    (progn (setf fraction 1d0) (pop targets)) (return)))))))
    (null targets)))

;;; Smooth completed routes conservatively. Rejected/incomplete/budget-limited windows retain
;;; re-sailed original TWA controls; any failed whole-route validation restores the original.
(defun fine-tune-route (routing info)
  (let* ((original (routeinfo-best info))
         (original-stats (routeinfo-stats info))
         (*fine-tune-evaluations* 0) (*fine-tune-started* (get-internal-real-time))
         (*fine-tune-weather* (make-hash-table :test #'equal))
         (report (make-fine-tuning :status "unchanged" :reason "disabled"
                   :evaluations 0
                   :original-changes (fine-changes original) :optimized-changes (fine-changes original)
                   :original-arrival (when original (trackpoint-time (car (last original))))
                   :optimized-arrival (when original (trackpoint-time (car (last original)))))))
    (setf (routeinfo-twa-segments info) (fine-segments original) (routeinfo-fine-tuning info) report)
    (unless (and *fine-tune-enabled* (string= (routeinfo-status info) "reached") (rest original))
      (return-from fine-tune-route info))
    (handler-case
        (block optimize
          (let ((track nil) (pos (trackpoint-position (first original))) (improvements 0))
            (loop for (window . more) on (fine-windows routing original) do
              (let* ((candidate (when (or (> (fine-changes window) 1) (fine-manoeuvres window) (null more))
                                  (fine-optimize-window routing window pos (null more))))
                     (baseline (unless candidate
                                 (fine-replay routing pos (trackpoint-time (first window)) (fine-controls window)
                                              :step nil :counted nil))))
                (when (and candidate (>= (fine-window-score window candidate) (fine-window-score window window))
                           (timestamp= (trackpoint-time (car (last candidate))) (trackpoint-time (car (last window)))))
                  (setf candidate nil baseline
                        (fine-replay routing pos (trackpoint-time (first window)) (fine-controls window) :step nil :counted nil)))
                (let ((chosen (or candidate baseline)))
                  (unless (fine-valid-window-p window chosen)
                    (setf (fine-tuning-reason report) "window could not be re-sailed within constraints")
                    (return-from optimize nil))
                  (when candidate (incf improvements))
                  (setf pos (trackpoint-position (car (last chosen)))
                        track (append (butlast track) chosen)))))
            (multiple-value-bind (valid space seconds) (fine-check-manoeuvres original track)
              (unless (and valid (fine-check-targets routing track)
                           (<= (fine-changes track) (fine-changes original))
                           (timestamp<= (trackpoint-time (car (last track))) (trackpoint-time (car (last original))))
                           (<= (fine-distance (trackpoint-position (car (last track)))
                                              (trackpoint-position (car (last original)))) *fine-tune-endpoint-tolerance*))
                (setf (fine-tuning-reason report) "whole-route validation failed")
                (return-from optimize nil))
              (when (plusp improvements)
                (let ((stats (routeinfo-stats info)))
                  (setf (routeinfo-original-best info) original (routeinfo-best info) track
                        (routeinfo-twa-segments info) (fine-segments track)
                        (routeinfo-stats info) (get-statistics track
                          (+ (routestats-calctime stats)
                             (/ (- (get-internal-real-time) *fine-tune-started*) internal-time-units-per-second))
                          (routestats-steps stats) (routestats-points stats))
                        (fine-tuning-status report) "optimized"
                        (fine-tuning-max-manoeuvre-distance report) space
                        (fine-tuning-max-manoeuvre-seconds report) seconds)))
              (setf (fine-tuning-reason report) (if (fine-budget-p) "validated" "trial budget reached")))))
      (error (e)
        (log2:warning "TWA fine-tuning retained original route: ~a" e)
        (setf (routeinfo-best info) original
              (routeinfo-stats info) original-stats
              (routeinfo-original-best info) nil
              (routeinfo-twa-segments info) (fine-segments original)
              (fine-tuning-status report) "unchanged"
              (fine-tuning-reason report) "post-processing unavailable")))
    (setf (fine-tuning-evaluations report) *fine-tune-evaluations*
          (fine-tuning-optimized-changes report) (fine-changes (routeinfo-best info))
          (fine-tuning-optimized-arrival report) (when original (trackpoint-time (car (last (routeinfo-best info))))))
    info))
