;;; Constrained steering refinement after the isochrone search. Commands are
;;; integer TWA or explicitly tagged compass heading, plus elapsed seconds.
(in-package :bitsailor)

(defparameter *fine-tune-enabled* t "Apply bounded steering post-processing to reached/max_duration routes.")
(defparameter *fine-tune-outward-enabled* t "Allow computed anchors to advance within a bounded lateral corridor.")
(defvar *fine-tune-progress-origin* nil)
(defvar *fine-tune-outward-window* nil)
(defvar *fine-tune-suffix-only* nil)
(defparameter *fine-tune-approximation-enabled* t "Try broader same-tack route approximations.")
(defparameter *fine-tune-approximation-limit* 1852d0 "Lateral approximation corridor, metres (one nautical mile).")
(defparameter *fine-tune-approximation-evaluations* 1000 "Route trial budget reserved for broader approximations.")
(defvar *fine-tune-approximation* nil)
(defvar *fine-tune-manoeuvre-limit* nil)
(defparameter *fine-tune-heading-enabled* t "Try integer compass-heading fits on same-tack stretches.")
(defparameter *fine-tune-heading-evaluations* 80 "Additional trial budget per eligible heading-fit window.")
(defparameter *fine-tune-step* 60d0 "Maximum integration step in seconds for trial TWA segments.")
(defparameter *fine-tune-window* 3600d0 "Maximum ordinary shooting-window duration in seconds.")
(defparameter *fine-tune-angle-limit* 5d0
  "Maximum TWA departure from a window's original angle range, not a step size.
Fine-tuning tries integer TWA values independently of the search's *TWA-STEPS*.")
(defparameter *fine-tune-position-limit* 500d0 "Maximum lateral corridor, or backward position displacement, in metres.")
(defparameter *fine-tune-time-limit* 300d0 "Maximum tack/gybe time displacement in seconds.")
(defparameter *fine-tune-endpoint-tolerance* 1d0 "Acceptance tolerance for fixed endpoints and target touches, in metres.")
(defparameter *fine-tune-fit-tolerance* 0.01d0 "Internal shooting closure tolerance in metres, tighter than validation.")
(defparameter *fine-tune-max-window-evaluations* 240 "Maximum trial integrations spent on one window.")
(defvar *fine-tune-local-limit* nil)
(defparameter *fine-tune-max-evaluations* 3000 "Maximum trial integrations per route.")
(defparameter *fine-tune-max-seconds* 10d0 "Wall-clock trial budget; checked between integrations.")
(defvar *fine-tune-evaluations* 0)
(defvar *fine-tune-started* 0)
(defvar *fine-tune-weather* nil)

(defstruct twa-segment start-time end-time twa duration)
(defstruct steering-segment mode value start-time end-time duration)
(defstruct fine-tuning status reason evaluations original-changes optimized-changes
  original-arrival optimized-arrival max-manoeuvre-distance max-manoeuvre-seconds
  (twa-step 1) integer-controls windows optimized-windows failed-window endpoint-error heading-segments heading-enabled
  objective original-origin-distance optimized-origin-distance distance-gain outward-anchors approximation corridor-limit original-destination-distance optimized-destination-distance)
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

;;; Spherical coordinates relative to the origin->reference ray. Along-track
;;; displacement distinguishes forward progress from the opposite half of the great circle.
;;; Return radial gain, along-track displacement and absolute cross-track distance, in metres.
(defun fine-progress-offset (reference candidate)
  (let* ((origin *fine-tune-progress-origin*)
         (r (course-distance origin reference))
         (d (course-distance origin candidate)))
    (if (< r 1d0)
        (values (- d r) 0d0 (fine-distance reference candidate))
        (let* ((delta (/ d 6371000d0))
               (angle (rad (normalize-angle (- (gate-course-angle origin candidate)
                                               (gate-course-angle origin reference)))))
               (lateral (* 6371000d0 (asin (max -1d0 (min 1d0 (* (sin delta) (sin angle)))))))
               (along (* 6371000d0 (atan (* (sin delta) (cos angle)) (cos delta)))))
          (values (- d r) (- along r) (abs lateral))))))

;;; Keep the old disk for backward movement, but extend it forward into a lateral tube.
(defun fine-position-admissible-p (reference candidate)
  (if (and *fine-tune-outward-enabled* *fine-tune-progress-origin*)
      (multiple-value-bind (gain along lateral) (fine-progress-offset reference candidate)
        (declare (ignore gain))
        (if (>= along (- *fine-tune-fit-tolerance*))
            (<= lateral *fine-tune-position-limit*)
            (<= (fine-distance reference candidate) *fine-tune-position-limit*)))
      (<= (fine-distance reference candidate) *fine-tune-position-limit*)))

;;; Free anchors retain their clock and corridor; only the final partial-route
;;; acceptance requires nonnegative net progress over the original complete track.
(defun fine-outward-endpoint-p (reference candidate)
  (and (timestamp= (trackpoint-time (car (last reference)))
                   (trackpoint-time (car (last candidate))))
       (fine-position-admissible-p (trackpoint-position (car (last reference)))
                                   (trackpoint-position (car (last candidate))))))

;;; Requested point waypoints and a completed route's terminal point are fixed constraints.
;;; Other endpoints are computational anchors, not new required destinations.
(defun fine-free-anchor-p (routing reference finalp partialp)
  (and *fine-tune-outward-enabled*
       (or (not *fine-tune-approximation*) finalp)
       (or (not finalp) partialp)
       (> (course-distance (routing-start routing) (trackpoint-position (car (last reference)))) 1d0)
       (not (some (lambda (w) (and (not (gate-p w))
                                  (< (fine-distance w (trackpoint-position (car (last reference)))) 1d0)))
                  (routing-waypoints routing)))))

;;; Compare outward candidates by radius first, then steering cost and lateral displacement.
;;; A tolerance-sized numerical radius difference must not trump a meaningful steering reduction.
(defun fine-outward-better-p (reference candidate old)
  (if (null old) t
      (multiple-value-bind (gain along lateral)
          (fine-progress-offset (trackpoint-position (car (last reference)))
                                (trackpoint-position (car (last candidate))))
        (declare (ignore along))
        (multiple-value-bind (old-gain old-along old-lateral)
            (fine-progress-offset (trackpoint-position (car (last reference)))
                                  (trackpoint-position (car (last old))))
          (declare (ignore old-along))
          (or (> gain (+ old-gain *fine-tune-fit-tolerance*))
              (and (<= (abs (- gain old-gain)) *fine-tune-fit-tolerance*)
                   (or (< (fine-window-score reference candidate) (fine-window-score reference old))
                       (and (= (fine-window-score reference candidate) (fine-window-score reference old))
                            (< lateral old-lateral)))))))))

;;; Legacy search samples have no explicit mode and represent signed TWA commands.
(defun fine-command-mode (point)
  (or (trackpoint-control-mode point) "twa"))

;;; Heading commands are compass headings, distinct from measured TWA and ground course.
(defun fine-command-value (point)
  (or (trackpoint-control-value point) (trackpoint-twa point)))

;;; Count commanded changes, not wind-induced changes of measured TWA under fixed heading.
(defun fine-changes (track)
  (loop for (a b c) on track while c
        count (or (not (string= (fine-command-mode a) (fine-command-mode b)))
                  (> (abs (normalize-angle (coerce (- (fine-command-value a) (fine-command-value b)) 'double-float))) 0.05d0))))

;;; Export unambiguous TWA/heading commands; coalesce equal commands despite changing wind.
(defun fine-steering-segments (track)
  (let ((result nil))
    (loop for (a b) on track while b
          for mode = (fine-command-mode a) for value = (fine-command-value a)
          when (timestamp< (trackpoint-time a) (trackpoint-time b)) do
      (if (and result (string= mode (steering-segment-mode (first result)))
               (< (abs (- value (steering-segment-value (first result)))) 1d-7))
          (setf (steering-segment-end-time (first result)) (trackpoint-time b)
                (steering-segment-duration (first result))
                (timestamp-difference (trackpoint-time b) (steering-segment-start-time (first result))))
          (push (make-steering-segment :mode mode :value value :start-time (trackpoint-time a)
                  :end-time (trackpoint-time b)
                  :duration (timestamp-difference (trackpoint-time b) (trackpoint-time a))) result)))
    (nreverse result)))

;;; Export a compact, executable signed-TWA plan. Equal consecutive angles are one segment.
(defun fine-segments (track)
  (when (some (lambda (p) (string= (fine-command-mode p) "heading")) track)
    (return-from fine-segments nil))
  (let ((result nil))
    (loop for (a b) on track while b
          for angle = (trackpoint-twa a)
          when (and (numberp angle) (timestamp< (trackpoint-time a) (trackpoint-time b))) do
      (if (and result (< (abs (- angle (twa-segment-twa (first result)))) 1d-7))
          (setf (twa-segment-end-time (first result)) (trackpoint-time b)
                (twa-segment-duration (first result))
                (timestamp-difference (trackpoint-time b) (twa-segment-start-time (first result))))
          (push (make-twa-segment :start-time (trackpoint-time a)
                  :end-time (trackpoint-time b) :twa angle
                  :duration (timestamp-difference (trackpoint-time b) (trackpoint-time a))) result)))
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
            seconds (max seconds (abs (timestamp-difference (fine-manoeuvre-time old) (fine-manoeuvre-time new)))))
      (unless (let ((*fine-tune-position-limit* (or *fine-tune-manoeuvre-limit* *fine-tune-position-limit*)))
                (fine-position-admissible-p (fine-manoeuvre-position old) (fine-manoeuvre-position new)))
        (return-from fine-check-manoeuvres nil)))
    (values (<= seconds *fine-tune-time-limit*) space seconds)))

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

;;; Advance TWA or (:HEADING compass-degrees), using local wind, polars, waves and current.
;;; Heading mode derives physical TWA anew; stored ground course includes current drift.
(defun fine-sail-step (routing pos time seconds control)
  (destructuring-bind (wind current wave wave-dir) (fine-weather routing time)
    (let ((lat (latlng-lat pos)) (lon (latlng-lng pos)) (polars (routing-polars routing)))
      (with-bindings
          (((twd tws) (interpolate-uv lat lon wind))
           ((twa) (values (if (consp control) (heading-twa twd (second control)) control)))
           ((up down) (best-vmg polars tws))
           ((speed sail) (twa-boatspeed polars tws twa))
           ((cd cs cu cv) (if current (interpolate-uv lat lon current) (values 0d0 0d0 0d0 0d0)))
           ((height direction) (if (and wave wave-dir)
                                  (values (interpolate-simple lat lon wave) (interpolate-direction lat lon wave-dir))
                                  (values nil nil))))
        (unless (and (> (abs twa) 0d0) (<= (vmg-twa up) (abs twa) (vmg-twa down))
                     (> speed 0.01d0))
          (return-from fine-sail-step nil))
        (let* ((heading (if (consp control) (float (second control) 1d0) (twa-heading twd twa)))
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
                    :control-mode (if (consp control) "heading" "twa")
                    :control-value (if (consp control) (second control) twa)
                    :wind-cycle (cl-weather:uv-cycle (params-fc1 wind))
                    :wind-model (routing-model-at routing time) :wind-time time)
                  (add-distance-estimate pos (* sog seconds) cog)))))))

;;; Reject trial integrations once the bounded post-processing budget has been spent.
(defun fine-budget-p ()
  (and (< *fine-tune-evaluations* *fine-tune-max-evaluations*)
       (or (null *fine-tune-local-limit*) (< *fine-tune-evaluations* *fine-tune-local-limit*))
       (< (/ (- (get-internal-real-time) *fine-tune-started*) internal-time-units-per-second)
          *fine-tune-max-seconds*)))

;;; Re-sail (command . seconds), where command is TWA or (:HEADING compass-degrees).
;;; Trial integration samples at most STEP seconds apart,
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
                          (if (consp (car control)) 60d0 remaining)
                          (if (and switch (timestamp< time switch))
                              (timestamp-difference switch time) remaining))))
            (multiple-value-bind (sample endpoint) (fine-sail-step routing pos time dt (car control))
              (unless sample (return-from fine-replay nil))
              (push sample track)
              (setf last-sample sample pos endpoint time (fine-time+ time dt))
              (decf remaining dt)
              (when (and (consp (car control)) (<= remaining 1d-7))
                ;; Check the heading immediately before every control switch,
                ;; including its endpoint, even if the next command is TWA.
                (let ((end (fine-sail-step routing pos time 0d0 (car control))))
                  (unless end (return-from fine-replay nil))
                  (push end track)
                  (setf last-sample end))))))))
    (when (and last-sample (not (timestamp= (trackpoint-time last-sample) time)))
      (let ((end (copy-trackpoint last-sample)))
        (setf (trackpoint-position end) pos (trackpoint-time end) time
              (trackpoint-dtf end) (course-distance pos (routing-dest routing)))
        (push end track)))
    (nreverse track)))

;;; Extract original controls without zero-time intersection duplicates.
(defun fine-controls (points)
  (loop for (a b) on points while b
        for dt = (timestamp-difference (trackpoint-time b) (trackpoint-time a))
        when (> dt 1d-7) collect
          (cons (if (string= (fine-command-mode a) "heading")
                    (list :heading (fine-command-value a))
                    (coerce (trackpoint-twa a) 'double-float))
                (coerce dt 'double-float))))

;;; Whole-degree steering is an application constraint, not output formatting.
(defun fine-integer-plan-p (track)
  (loop for (a b) on track while b
        always (and (numberp (fine-command-value a))
                    (= (fine-command-value a) (round (fine-command-value a))))))

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
       (if *fine-tune-outward-window* (fine-outward-endpoint-p reference candidate)
           (<= (fine-distance (trackpoint-position (car (last reference)))
                              (trackpoint-position (car (last candidate)))) *fine-tune-endpoint-tolerance*))
       (fine-check-manoeuvres reference candidate)
       (every (lambda (p) (fine-position-admissible-p
                          (fine-reference-position reference (trackpoint-time p))
                          (trackpoint-position p))) candidate)
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
          (when (<= (fourth base) (expt (min *fine-tune-fit-tolerance* *fine-tune-endpoint-tolerance*) 2))
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

;;; Candidate whole-degree controls, nearest the original time-weighted mean first.
;;; Retain nearby coarse angles and one-degree neighbours without sorting.
(defun fine-integer-angles (controls)
  (let* ((bounds (fine-angle-bounds controls))
         (mean (/ (loop for c in controls sum (* (car c) (cdr c))) (reduce #'+ controls :key #'cdr)))
         (values (list (floor mean) (ceiling mean)
                       (round (caar controls)) (round (car (car (last controls))))
                       (1- (floor (reduce #'min controls :key #'car)))
                       (1+ (ceiling (reduce #'max controls :key #'car)))
                       (1- (round mean)) (1+ (round mean)))))
    (remove-duplicates
     (remove-if-not (lambda (a) (and (not (zerop a)) (<= (car bounds) a (cdr bounds)))) values)
     :from-end t)))

;;; One-parameter least-squares shooting: two endpoint residuals, one split/duration.
;;; Accept only actual closure, never the nearest point of a non-closing trial.
(defun fine-shoot-one (routing pos time target builder seed low high probe)
  (labels ((evaluate-at (x)
             (when (<= low x high)
               (let ((track (fine-replay routing pos time (funcall builder x))))
                 (when track
                   (multiple-value-bind (east north)
                       (fine-offset target (trackpoint-position (car (last track))))
                     (list track east north (+ (* east east) (* north north)))))))))
    (let ((x seed))
      (loop repeat 8 do
        (let ((base (evaluate-at x)))
          (unless base (return-from fine-shoot-one nil))
          (when (<= (fourth base) (expt (min *fine-tune-fit-tolerance* *fine-tune-endpoint-tolerance*) 2))
            (return-from fine-shoot-one (first base)))
          (let* ((h (if (<= low (+ x probe) high) probe (- probe)))
                 (p (evaluate-at (+ x h))))
            (unless p (return-from fine-shoot-one nil))
            (let* ((dx (/ (- (second p) (second base)) h))
                   (dy (/ (- (third p) (third base)) h))
                   (square (+ (* dx dx) (* dy dy)))
                   (next nil))
              (when (< square 1d-12) (return-from fine-shoot-one nil))
              (let ((step (- (/ (+ (* dx (second base)) (* dy (third base))) square))))
                (dolist (factor '(1d0 0.5d0 0.25d0 0.125d0))
                  (let* ((v (max low (min high (+ x (* factor step))))) (trial (evaluate-at v)))
                    (when (and trial (< (fourth trial) (fourth base))) (setf next v) (return)))))
              (unless next (return-from fine-shoot-one nil))
              (setf x next))))))))

;;; Only small same-tack reversals qualify; monotone steering changes and manoeuvres do not.
(defun fine-heading-stretch-p (reference)
  (let* ((angles (loop for c in (fine-controls reference) collect (car c)))
         (compact (loop with previous = nil for a in angles
                        unless (eql a previous) collect a do (setf previous a))))
    (and (>= (length compact) 3) (every #'numberp angles)
         (null (fine-manoeuvres reference))
         (<= (- (reduce #'max angles) (reduce #'min angles)) (* 2d0 *fine-tune-angle-limit*))
         (loop for (a b c) on compact while c thereis (minusp (* (- b a) (- c b)))))))

;;; The broad pass also considers gradual same-tack steering, not just oscillation.
(defun fine-heading-eligible-p (reference)
  (or (fine-heading-stretch-p reference)
      (and *fine-tune-approximation* (null (fine-manoeuvres reference))
           (>= (timestamp-difference (trackpoint-time (car (last reference)))
                                     (trackpoint-time (first reference))) 600d0)
           (let ((angles (mapcar #'car (fine-controls reference))))
             (and angles (every #'numberp angles)
                  (every (lambda (a) (plusp (* a (first angles)))) angles))))))

;;; A fixed compass heading may change realized TWA, but it must stay close to the
;;; original stretch and on its original side of the wind throughout the integration.
(defun fine-heading-range-p (reference candidate)
  (let* ((angles (mapcar #'car (fine-controls reference)))
         (low (- (reduce #'min angles) *fine-tune-angle-limit*))
         (high (+ (reduce #'max angles) *fine-tune-angle-limit*)))
    (every (lambda (p) (<= low (trackpoint-twa p) high)) candidate)))

;;; Fit one whole-degree compass heading, optionally with short integer-TWA tails.
;;; ACCEPT performs the same land/corridor/manoeuvre/ETA validation as TWA fits.
;;; Heading occupies at least 75% of a conservative mixed trial, or 25% in
;;; the broader approximation pass; corrective TWA segments must close the anchor.
(defun fine-try-heading (routing reference pos finalp accept)
  (let* ((controls (fine-controls reference))
         (time (trackpoint-time (first reference)))
         (target (trackpoint-position (car (last reference))))
         (duration (timestamp-difference (trackpoint-time (car (last reference))) time))
         (bearing (gate-course-angle pos target))
         (compass
           (let ((x 0d0) (y 0d0))
             (loop for (a b) on reference while b
                   for dt = (timestamp-difference (trackpoint-time b) (trackpoint-time a))
                   for heading = (if (numberp (trackpoint-twd a))
                                     (twa-heading (trackpoint-twd a) (trackpoint-twa a)) bearing)
                   do (incf x (* dt (sin (rad heading)))) (incf y (* dt (cos (rad heading)))))
             (if (< (+ (abs x) (abs y)) 1d-9) bearing (mod (deg (atan x y)) 360d0))))
         (headings (remove-duplicates
                    (loop for centre in (list compass bearing) append
                      (loop for offset in '(0 1 -1 2 -2)
                            append (list (mod (+ (floor centre) offset) 360)
                                         (mod (+ (ceiling centre) offset) 360))))))
         (tails (fine-integer-angles controls))
         (low (floor (reduce #'min controls :key #'car)))
         (high (ceiling (reduce #'max controls :key #'car)))
         (*fine-tune-local-limit*
           (min *fine-tune-local-limit* (+ *fine-tune-evaluations* *fine-tune-heading-evaluations*))))
    (labels ((submit (candidate)
               (when (and candidate (fine-heading-range-p reference candidate)
                          (or *fine-tune-outward-window*
                              (<= (fine-distance target (trackpoint-position (car (last candidate))))
                                  (min *fine-tune-fit-tolerance* *fine-tune-endpoint-tolerance*))))
                 (funcall accept candidate))))
      (dolist (heading headings)
        (when (fine-budget-p)
          (submit
           (if finalp
               (fine-shoot-one routing pos time target
                 (lambda (dt) (list (cons (list :heading heading) dt)))
                 (* duration 0.999d0) (* duration 0.8d0) duration -0.1d0)
               (fine-replay routing pos time (list (cons (list :heading heading) duration)))))))
      (if finalp
          (dolist (heading headings)
            (dolist (twa tails)
              (when (fine-budget-p)
                (submit (fine-shoot routing pos time target
                          (lambda (v) (list (cons (list :heading heading) (* (first v) (second v)))
                                            (cons (float twa 1d0) (* (first v) (- 1d0 (second v))))))
                          (list (* duration 0.999d0) 0.95d0)
                          (list (cons (* duration 0.8d0) duration) (cons (if *fine-tune-approximation* 0.25d0 0.75d0) 0.999999d0))
                          '(-0.1d0 0.0001d0))))))
          (dolist (heading headings)
            (dolist (pair (list (list low high) (list (1- low) high) (list low (1+ high))
                                (list high low) (list (1+ high) low)))
              (when (and (fine-budget-p) (member (first pair) tails) (member (second pair) tails)
                         (/= (first pair) (second pair)))
                (submit (fine-shoot routing pos time target
                          (lambda (v)
                            (when (< (+ (first v) (second v)) 0.999999d0)
                              (list (cons (list :heading heading) (* duration (first v)))
                                    (cons (float (first pair) 1d0) (* duration (second v)))
                                    (cons (float (second pair) 1d0) (* duration (- 1d0 (first v) (second v)))))))
                          (if *fine-tune-approximation* '(0.5d0 0.25d0) '(0.85d0 0.075d0))
                          (list (cons (if *fine-tune-approximation* 0.25d0 0.75d0) 0.999998d0)
                                (cons 0.000001d0 (if *fine-tune-approximation* 0.749999d0 0.249999d0)))
                          '(0.0001d0 0.0001d0))))))))))

;;; free pairs use the fixed original duration while varying the split.
(defun fine-try-outward-pair (routing reference pos a b duration split accept)
  (let ((time (trackpoint-time (first reference))) (best nil) (best-fraction split))
    (labels ((try (fraction)
               (when (and (fine-budget-p) (< 0.000001d0 fraction 0.999999d0))
                 (let ((candidate (fine-replay routing pos time
                                    (list (cons (float a 1d0) (* duration fraction))
                                          (cons (float b 1d0) (* duration (- 1d0 fraction)))))))
                   (when (funcall accept candidate)
                     (when (fine-outward-better-p reference candidate best)
                       (setf best candidate best-fraction fraction)))))))
      (dolist (f (remove-duplicates (list split 0.25d0 0.5d0 0.75d0))) (try f))
      (dolist (step '(0.05d0 0.01d0 0.002d0))
        (let ((centre best-fraction)) (try (- centre step)) (try (+ centre step)))))))

;;; Fit integer TWA values by solving durations. Intermediate windows keep their
;;; total time; a third integer segment supplies the second degree of freedom.
;;; Final windows can adjust both duration and split, but can never finish later.
(defun fine-optimize-window (routing reference pos finalp)
  (let* ((controls (fine-controls reference))
         (time (trackpoint-time (first reference)))
         (target (trackpoint-position (car (last reference))))
         (duration (timestamp-difference (trackpoint-time (car (last reference))) time))
         (events (fine-manoeuvres reference))
         (event (first events))
         (before (if event (loop for c in controls until (minusp (* (caar controls) (car c))) collect c) controls))
         (after (if event (nthcdr (length before) controls) controls))
         (left (fine-integer-angles before)) (right (fine-integer-angles after))
         (split (if event (/ (timestamp-difference (fine-manoeuvre-time event) time) duration) 0.5d0))
         (best nil) (score most-positive-double-float)
         (heading-p (and *fine-tune-heading-enabled* (null events) (fine-heading-eligible-p reference)))
         (*fine-tune-local-limit* (+ *fine-tune-evaluations* *fine-tune-max-window-evaluations*
                                    (if heading-p *fine-tune-heading-evaluations* 0))))
    (when (or (null controls) (> (length events) 1)) (return-from fine-optimize-window nil))
    (labels ((consider (track)
               (when (and (fine-valid-window-p reference track)
                          (timestamp<= (trackpoint-time (car (last track)))
                                       (trackpoint-time (car (last reference)))))
                 (let ((value (fine-window-score reference track)))
                   (when (if *fine-tune-outward-window*
                             (fine-outward-better-p reference track best)
                             (or (null best)
                                 (and finalp (timestamp< (trackpoint-time (car (last track)))
                                                        (trackpoint-time (car (last best)))))
                                 (and (timestamp= (trackpoint-time (car (last track)))
                                                  (trackpoint-time (car (last best))))
                                      (< value score))))
                     (setf best track score value))
                   t)))
             (triple (a b c f g)
               (when (and (fine-budget-p) (/= a b) (/= b c))
                 (consider (fine-shoot routing pos time target
                             (lambda (v)
                               (when (< (+ (first v) (second v)) 0.999999d0)
                                 (list (cons (float a 1d0) (* duration (first v)))
                                       (cons (float b 1d0) (* duration (second v)))
                                       (cons (float c 1d0) (* duration (- 1d0 (first v) (second v)))))))
                             (list f g) (list (cons 0.000001d0 0.999998d0) (cons 0.000001d0 0.999998d0))
                             '(0.0001d0 0.0001d0))))))
      (when *fine-tune-outward-window*
        (consider (fine-replay routing pos time controls :step nil :counted nil)))
      ;; Repeated A/B/A controls provide two independent switch times in
      ;; rotating wind. The former edge/middle/edge generator always used
      ;; different edge angles, omitting this useful correction pattern.
      ;; Compress an oscillating prefix while allowing following segments to
      ;; cancel its endpoint error at the window's original position and time.
      (when (and (null events) (fine-heading-eligible-p reference)
                 (not finalp) (not *fine-tune-outward-window*))
        (let ((low (floor (reduce #'min controls :key #'car)))
              (high (ceiling (reduce #'max controls :key #'car))))
          (triple low high low 0.2d0 0.2d0)
          (triple high low high 0.2d0 0.2d0)))
      (when heading-p (fine-try-heading routing reference pos finalp #'consider))
      ;; Single integer-TWA segments are considered directly, never by rounding a fit.
      (unless event
        (dolist (a left)
          (when (fine-budget-p)
            (consider
             (if finalp
                 (fine-shoot-one routing pos time target
                   (lambda (dt) (list (cons (float a 1d0) dt)))
                   (* duration 0.999d0) (* duration 0.8d0) duration -0.1d0)
                 (fine-replay routing pos time (list (cons (float a 1d0) duration))))))))
      ;; Try a small deterministic set of nearest-angle pairs before three segments.
      (let ((count 0))
        (dolist (a left)
          (dolist (b right)
            (when (and (/= a b) (< count (if *fine-tune-outward-window* 12 (if finalp 36 3))) (fine-budget-p))
              (incf count)
              (if *fine-tune-outward-window*
                  (fine-try-outward-pair routing reference pos a b duration split #'consider)
                  (consider
                   (if finalp
                       (fine-shoot routing pos time target
                         (lambda (v) (list (cons (float a 1d0) (* (first v) (second v)))
                                           (cons (float b 1d0) (* (first v) (- 1d0 (second v))))))
                         (list (* duration 0.999d0) split)
                         (list (cons (* duration 0.8d0) duration) (cons 0.000001d0 0.999999d0))
                         '(-0.1d0 0.0001d0))
                       (fine-shoot-one routing pos time target
                         (lambda (f) (list (cons (float a 1d0) (* duration f))
                                           (cons (float b 1d0) (* duration (- 1d0 f)))))
                         split 0.000001d0 0.999999d0 0.0001d0))))))))
      ;; For equal-time windows, two independently adjustable durations among
      ;; three fixed TWAs can close both geographic coordinates in rotating wind.
      (unless (and best (not finalp))
        (if event
            (progn
              (dolist (a left)
                (dolist (b right)
                  (dolist (c left) (triple a c b (* split 0.5d0) (* split 0.5d0)))
                  (dolist (c right) (triple a b c split (* (- 1d0 split) 0.5d0))))))
            (let* ((low (floor (reduce #'min controls :key #'car)))
                   (high (ceiling (reduce #'max controls :key #'car))))
              (dolist (middle left)
                (dolist (edges (list (list (1- low) high) (list low (1+ high)) (list low high)))
                  (when (and (member (first edges) left) (member (second edges) left))
                    (triple (first edges) middle (second edges) 0.3d0 0.4d0)
                    (triple (second edges) middle (first edges) 0.3d0 0.4d0)))))))
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

;;; Broad fitting starts/ends at command boundaries where possible, and never
;;; spans a tack/gybe or requested point waypoint. Residuals must still close.
(defun fine-approximation-windows (routing track)
  (let ((result nil) (remaining track))
    (loop while (rest remaining) do
      (let ((end 1) (boundary nil) (hard nil) (start (trackpoint-time (first remaining))))
        (loop for (a b c) on remaining while b for i from 1 do
          (let* ((elapsed (timestamp-difference (trackpoint-time b) start))
                 (change (or (not (string= (fine-command-mode a) (fine-command-mode b)))
                             (/= (fine-command-value a) (fine-command-value b)))))
            (when (> elapsed *fine-tune-window*) (return))
            (setf end i)
            (when (and c change (>= elapsed 600d0)) (setf boundary i))
            (when (or (minusp (* (trackpoint-twa a) (trackpoint-twa b)))
                      (some (lambda (w) (and (not (gate-p w))
                                            (< (fine-distance w (trackpoint-position b)) 1d0)))
                            (routing-waypoints routing)))
              (setf hard t) (return))))
        (unless (or hard (= end (1- (length remaining))))
          (when boundary (setf end boundary)))
        (push (subseq remaining 0 (1+ end)) result)
        (setf remaining (nthcdr end remaining))))
    (nreverse result)))

;;; Verify ordered targets over actual segments. Touches within closure tolerance count;
;;; multiple gates in one segment must occur in the requested spatial order.
(defun fine-target-progress (routing track)
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
    (- (length (routing-waypoints routing)) (length targets))))

;;; Completed routes require every target; partial routes retain at least their original prefix.
(defun fine-check-targets (routing track &optional required)
  (>= (fine-target-progress routing track) (or required (length (routing-waypoints routing)))))

;;; Validate the original terminal constraint or the fixed-horizon radial objective.
(defun fine-result-valid-p (routing original track partialp)
  (and (fine-integer-plan-p track)
       (fine-check-targets routing track
         (if partialp (fine-target-progress routing original) (length (routing-waypoints routing))))
       (if partialp
           (and (timestamp= (trackpoint-time (car (last track))) (trackpoint-time (car (last original))))
                (multiple-value-bind (gain along lateral)
                    (fine-progress-offset (trackpoint-position (car (last original)))
                                          (trackpoint-position (car (last track))))
                  (and (<= (course-distance (routing-dest routing) (trackpoint-position (car (last track))))
                                 (course-distance (routing-dest routing) (trackpoint-position (car (last original)))))
                       (>= gain 0d0) (>= along (- *fine-tune-fit-tolerance*))
                       (<= lateral *fine-tune-position-limit*)
                       (or (> gain *fine-tune-fit-tolerance*) (<= (fine-changes track) (fine-changes original))))))
           (and (timestamp<= (trackpoint-time (car (last track))) (trackpoint-time (car (last original))))
                (<= (fine-changes track) (fine-changes original))
                (<= (fine-distance (trackpoint-position (car (last track)))
                                   (trackpoint-position (car (last original)))) *fine-tune-endpoint-tolerance*)))))

;;; Refine reached or duration-limited routes. Rejected/budget-limited windows retain
;;; re-sailed original TWA controls; any failed whole-route validation restores the original.
(defun fine-tune-route-pass (routing info)
  (let* ((original (routeinfo-best info))
         (original-stats (routeinfo-stats info))
         (partialp (string= (routeinfo-status info) "max_duration"))
         (*fine-tune-progress-origin* (routing-start routing))
         (*fine-tune-evaluations* 0) (*fine-tune-started* (get-internal-real-time))
         (*fine-tune-weather* (make-hash-table :test #'equal))
         (report (make-fine-tuning :status "unchanged" :reason "disabled"
                   :evaluations 0 :outward-anchors *fine-tune-outward-enabled*
                   :approximation *fine-tune-approximation* :corridor-limit *fine-tune-position-limit*
                   :original-destination-distance (when original (course-distance (routing-dest routing) (trackpoint-position (car (last original)))))
                   :objective (if partialp "origin-distance" "earliest-arrival")
                   :original-origin-distance (when original (course-distance (routing-start routing) (trackpoint-position (car (last original)))))
                   :heading-enabled *fine-tune-heading-enabled* :heading-segments 0
                   :windows 0 :optimized-windows 0 :integer-controls (fine-integer-plan-p original)
                   :original-changes (fine-changes original) :optimized-changes (fine-changes original)
                   :original-arrival (when original (trackpoint-time (car (last original))))
                   :optimized-arrival (when original (trackpoint-time (car (last original)))))))
    (setf (fine-tuning-optimized-destination-distance report) (fine-tuning-original-destination-distance report)
          (fine-tuning-optimized-origin-distance report) (fine-tuning-original-origin-distance report)
          (fine-tuning-distance-gain report) (when original 0d0))
    (setf (routeinfo-twa-segments info) (when (fine-integer-plan-p original) (fine-segments original))
          (routeinfo-fine-tuning info) report
          (routeinfo-steering-segments info) (when (fine-integer-plan-p original) (fine-steering-segments original)))
    (unless (and *fine-tune-enabled* (or (string= (routeinfo-status info) "reached")
                        (and partialp *fine-tune-outward-enabled*)) (rest original))
      (setf (fine-tuning-reason report)
            (cond ((not *fine-tune-enabled*) "disabled")
                  ((and partialp (not *fine-tune-outward-enabled*)) "outward refinement disabled")
                  ((not (member (routeinfo-status info) '("reached" "max_duration") :test #'string=)) "route not completed")
                  (t "route too short")))
      (return-from fine-tune-route-pass info))
    (setf (fine-tuning-reason report) "no feasible integer improvement")
    (handler-case
        (block optimize
          (let ((track nil) (pos (trackpoint-position (first original))) (improvements 0))
            (loop for (window . more) on (if *fine-tune-approximation*
                                            (fine-approximation-windows routing original)
                                            (fine-windows routing original))
                  for index from 1 do
              (setf (fine-tuning-windows report) index)
              (let* ((*fine-tune-outward-window* (and (or (not *fine-tune-suffix-only*) (null more))
                                                           (fine-free-anchor-p routing window (null more) partialp)))
                     (candidate (when (and (or (not *fine-tune-suffix-only*) (null more))
                                           (or *fine-tune-outward-window* (> (fine-changes window) 1) (fine-manoeuvres window) (null more)))
                                  (fine-optimize-window routing window pos (and (null more) (not partialp)))))
                     (baseline (unless candidate
                                 (fine-replay routing pos (trackpoint-time (first window)) (fine-controls window)
                                              :step nil :counted nil))))
                (when (and candidate (not *fine-tune-outward-window*)
                           (>= (fine-window-score window candidate) (fine-window-score window window))
                           (timestamp= (trackpoint-time (car (last candidate))) (trackpoint-time (car (last window)))))
                  (setf candidate nil baseline
                        (fine-replay routing pos (trackpoint-time (first window)) (fine-controls window) :step nil :counted nil)))
                (let ((chosen (or candidate baseline)))
                  (unless (fine-valid-window-p window chosen)
                    (setf (fine-tuning-reason report) "window could not be re-sailed within constraints"
                          (fine-tuning-failed-window report) index
                          (fine-tuning-endpoint-error report)
                          (when chosen (fine-distance (trackpoint-position (car (last window)))
                                                       (trackpoint-position (car (last chosen))))))
                    (return-from optimize nil))
                  (when candidate (incf improvements))
                  (setf pos (trackpoint-position (car (last chosen)))
                        track (append (butlast track) chosen)))))
            (multiple-value-bind (valid space seconds) (fine-check-manoeuvres original track)
              (unless (and valid (fine-result-valid-p routing original track partialp))
                (setf (fine-tuning-reason report) "whole-route validation failed")
                (return-from optimize nil))
              ;; A re-sailed baseline is a feasible candidate, not an improvement
              ;; merely because its points are new objects.
              (when (and partialp
                         (<= (- (course-distance (routing-start routing) (trackpoint-position (car (last track))))
                                (fine-tuning-original-origin-distance report)) *fine-tune-fit-tolerance*)
                         (>= (fine-window-score original track) (fine-window-score original original)))
                (setf improvements 0))
              (when (plusp improvements)
                (let ((stats (routeinfo-stats info)))
                  (setf (routeinfo-original-best info) original (routeinfo-best info) track
                        (routeinfo-twa-segments info) (fine-segments track)
                        (routeinfo-steering-segments info) (fine-steering-segments track)
                        (fine-tuning-heading-segments report)
                        (count "heading" (fine-steering-segments track) :key #'steering-segment-mode :test #'string=)
                        (routeinfo-stats info) (get-statistics track
                          (+ (routestats-calctime stats)
                             (/ (- (get-internal-real-time) *fine-tune-started*) internal-time-units-per-second))
                          (routestats-steps stats) (routestats-points stats))
                        (fine-tuning-status report) "optimized"
                        (fine-tuning-optimized-windows report) improvements
                        (fine-tuning-max-manoeuvre-distance report) space
                        (fine-tuning-max-manoeuvre-seconds report) seconds)))
              (setf (fine-tuning-reason report)
                    (cond ((not (fine-budget-p)) "trial budget reached")
                          ((plusp improvements) "validated")
                          (t "no feasible integer improvement"))))))
      (error (e)
        (log2:warning "TWA fine-tuning retained original route: ~a" e)
        (setf (routeinfo-best info) original
              (routeinfo-stats info) original-stats
              (routeinfo-original-best info) nil
              (routeinfo-twa-segments info) (when (fine-integer-plan-p original) (fine-segments original))
              (routeinfo-steering-segments info) (when (fine-integer-plan-p original) (fine-steering-segments original))
              (fine-tuning-status report) "unchanged"
              (fine-tuning-reason report) "post-processing unavailable")))
    (setf (fine-tuning-evaluations report) *fine-tune-evaluations*
          (fine-tuning-integer-controls report) (fine-integer-plan-p (routeinfo-best info))
          (fine-tuning-optimized-changes report) (fine-changes (routeinfo-best info))
          (fine-tuning-optimized-arrival report) (when original (trackpoint-time (car (last (routeinfo-best info))))))
    (setf (fine-tuning-optimized-destination-distance report)
          (when original (course-distance (routing-dest routing) (trackpoint-position (car (last (routeinfo-best info)))))))
    (setf (fine-tuning-optimized-origin-distance report)
          (when original (course-distance (routing-start routing) (trackpoint-position (car (last (routeinfo-best info))))))
          (fine-tuning-distance-gain report)
          (when original (- (fine-tuning-optimized-origin-distance report) (fine-tuning-original-origin-distance report))))
    info))

;;; Keep a conservative incumbent before speculative outward propagation: closed
;;; anchors for completed routes, or an unchanged prefix plus a refined final window
;;; for partial routes. Compare by ETA/steering or fixed-horizon origin distance.
;;; Both passes share the existing route-level evaluation/time budgets.
(defun fine-tune-route (routing info)
  (unless (and *fine-tune-enabled* *fine-tune-outward-enabled*
               (member (routeinfo-status info) '("reached" "max_duration") :test #'string=)
               (rest (routeinfo-best info)))
    (return-from fine-tune-route (fine-tune-route-pass routing info)))
  (let* ((started (get-internal-real-time))
         (search-seconds (routestats-calctime (routeinfo-stats info)))
         (partialp (string= (routeinfo-status info) "max_duration"))
         (baseline (let ((*fine-tune-outward-enabled* partialp)
                         (*fine-tune-suffix-only* partialp)
                         (*fine-tune-max-seconds* (if *fine-tune-approximation-enabled*
                                                     (* 0.5d0 *fine-tune-max-seconds*) *fine-tune-max-seconds*))
                         (*fine-tune-max-evaluations*
                           (if *fine-tune-approximation-enabled*
                               (max 0 (- *fine-tune-max-evaluations*
                                         (min (floor *fine-tune-max-evaluations* 2) *fine-tune-approximation-evaluations*)))
                               *fine-tune-max-evaluations*)))
                     (fine-tune-route-pass routing (copy-routeinfo info))))
         (used (fine-tuning-evaluations (routeinfo-fine-tuning baseline)))
         (remaining-seconds (- *fine-tune-max-seconds*
                               (/ (- (get-internal-real-time) started) internal-time-units-per-second)))
         (outward (when (and (> remaining-seconds 0d0) (< used *fine-tune-max-evaluations*))
                    (let ((*fine-tune-max-evaluations* (- *fine-tune-max-evaluations* used))
                          (*fine-tune-max-seconds* remaining-seconds)
                          (*fine-tune-approximation* *fine-tune-approximation-enabled*)
                          (*fine-tune-manoeuvre-limit* *fine-tune-position-limit*)
                          (*fine-tune-position-limit* (if *fine-tune-approximation-enabled*
                                                        *fine-tune-approximation-limit* *fine-tune-position-limit*)))
                      (fine-tune-route-pass routing (copy-routeinfo info)))))
         (chosen baseline))
    (when outward
      (let* ((a (routeinfo-best baseline)) (b (routeinfo-best outward))
             (ta (trackpoint-time (car (last a)))) (tb (trackpoint-time (car (last b)))))
        (when (and (string= (fine-tuning-status (routeinfo-fine-tuning outward)) "optimized")
                   (if partialp
                       (let ((da (course-distance (routing-start routing) (trackpoint-position (car (last a)))))
                             (db (course-distance (routing-start routing) (trackpoint-position (car (last b))))))
                         (and (<= (course-distance (routing-dest routing) (trackpoint-position (car (last b))))
                                  (course-distance (routing-dest routing) (trackpoint-position (car (last a)))))
                              (or (> db (+ da *fine-tune-fit-tolerance*))
                             (and (>= db da) (<= (fine-window-score (routeinfo-best info) b)
                                                (fine-window-score (routeinfo-best info) a))))))
                       (or (timestamp< tb ta)
                           (and (timestamp= ta tb)
                                (<= (fine-window-score (routeinfo-best info) b)
                                    (fine-window-score (routeinfo-best info) a))))))
          (setf chosen outward))))
    (setf (fine-tuning-evaluations (routeinfo-fine-tuning chosen))
          (+ used (if outward (fine-tuning-evaluations (routeinfo-fine-tuning outward)) 0)))
    (setf (routeinfo-best info) (routeinfo-best chosen)
          (routeinfo-stats info) (copy-routestats (routeinfo-stats chosen))
          (routeinfo-original-best info) (routeinfo-original-best chosen)
          (routeinfo-twa-segments info) (routeinfo-twa-segments chosen)
          (routeinfo-steering-segments info) (routeinfo-steering-segments chosen)
          (routeinfo-fine-tuning info) (routeinfo-fine-tuning chosen))
    (setf (routestats-calctime (routeinfo-stats info))
          (+ search-seconds (/ (- (get-internal-real-time) started) internal-time-units-per-second)))
    (when (>= (fine-tuning-evaluations (routeinfo-fine-tuning info)) *fine-tune-max-evaluations*)
      (setf (fine-tuning-reason (routeinfo-fine-tuning info)) "trial budget reached"))
    (bt:with-lock-held (+last-routestats-lock+) (setf *last-routestats* (routeinfo-stats info)))
    info))
