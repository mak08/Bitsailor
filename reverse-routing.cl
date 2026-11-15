(in-package :bitsailor)

(defconstant +reverse-overlap-tolerance+ 0.1d0)

(defstruct reverse-result status baseline deadline percent incomplete-at (steps 0) (candidates 0) (isochrones #()) (overlap #()))
(defstruct reverse-slice start end cycle cycles display paths)
(defstruct reverse-run slices frontier history stepper)

(defun reverse-hour-p (time)
  "Identify the UTC hourly boundaries shared by both passes."
  (zerop (mod (timestamp-to-unix time) 3600)))

(defun reverse-field-cycle (params)
  "Retain the source timestamp alone, so old decoded forecasts can be evicted."
  (when params
    (let ((field (params-fc1 params)))
      (etypecase field
        (cl-weather::uv (cl-weather:uv-cycle field))
        (cl-weather::gribmessage (cl-weather::gribmessage-basetime field))))))

(defun reverse-weather (routing time cycles)
  "Reopen the wind, current, height and direction runs actually used by the forward interval."
  (append
   (loop for cycle in cycles
         for source in (list (routing-model-at routing time) (routing-currents routing))
         collect (when cycle
                   (cl-weather::get-params-for-cycle source time
                     :cycle (cl-weather::make-cycle% :timestamp cycle))))
   (if (third cycles)
       (multiple-value-list (routing-wave-params routing time
                              :cycle (cl-weather::make-cycle% :timestamp (third cycles)) :fallback nil))
       (list nil nil))))

(defun reverse-blocked-p (a b)
  "Check the whole sailed or displayed segment across map tiles, including the date line."
  (let* ((lat (latlng-lat a)) (lon (normalize-angle (latlng-lng a)))
         (dlat (- (latlng-lat b) lat)) (dlon (normalize-angle (- (latlng-lng b) lon)))
         (spacing (if *use-bitmap*
                      (* 0.5d0 (min (cl-map::bitmap-dlat% cl-map::*bitmap*)
                                    (cl-map::bitmap-dlon% cl-map::*bitmap*)))
                      cl-map::+tile-width+))
         (cuts (list 0d0 1d0)))
    (flet ((crossings (start delta)
             (unless (zerop delta)
               (loop for i from (1+ (floor (min start (+ start delta)) spacing))
                     below (ceiling (max start (+ start delta)) spacing)
                     do (push (/ (- (* i spacing) start) delta) cuts))))
           (at (f)
             (make-latlng :lat (+ lat (* f dlat))
                          :lng (- (mod (+ 180d0 lon (* f dlon)) 360d0) 180d0))))
      (if (and (= (floor lat spacing) (floor (+ lat dlat) spacing))
               (= (floor lon spacing) (floor (+ lon dlon) spacing)))
          (approach-blocked-p a b)
          (progn
            (crossings lat dlat) (crossings lon dlon)
            (loop for (lo hi) on (sort cuts #'<) while hi
                  thereis (and (< lo hi)
                               (let* ((mid (at (/ (+ lo hi) 2))) (start (at lo)) (end (at hi)))
                                 ;; End each half inside its own tile for the vector map fast path.
                                 ;; Keep the date-line endpoint on the same side as its half-step.
                                 (when (> (abs (- (latlng-lng start) (latlng-lng mid))) 180d0)
                                   (setf start (make-latlng :lat (latlng-lat start) :lng 179.999999999d0)))
                                 (when (> (abs (- (latlng-lng end) (latlng-lng mid))) 180d0)
                                   (setf end (make-latlng :lat (latlng-lat end) :lng 179.999999999d0)))
                                 (or (approach-blocked-p start mid) (approach-blocked-p end mid)
                                     (and *use-bitmap* (bm-is-land end)))))))))))

(defun reverse-save-step (run routing start end weather frontier history stepper)
  "Save clock/cycle for every step and compact positions at comparison times."
  (let* ((age (timestamp-difference end (routing-starttime routing)))
         (save (or (null (reverse-run-slices run)) (reverse-hour-p end)
                   (and (< age 3600) (zerop (mod (timestamp-to-unix end) 600))))))
    (push (make-reverse-slice :start start :end end :cycle (cl-weather:uv-cycle (params-fc1 (first weather)))
            :cycles (mapcar #'reverse-field-cycle weather) :display save
            :paths (when save (reverse-forward-paths frontier))) (reverse-run-slices run))
    (setf (reverse-run-frontier run) frontier (reverse-run-history run) history
          (reverse-run-stepper run) stepper)))

(defun reverse-forward-paths (frontier)
  "Keep adjacent occupied buckets in the forward fan; break at gaps and land."
  (let ((previous nil) (path nil) (paths nil) (width (/ 360d0 *max-iso-points*)))
    (loop for point across frontier for pos = (routepoint-position point) do
      (when (and previous
                 (or (/= 1 (mod (- (bucket (routepoint-origin-angle point) width)
                                    (bucket (routepoint-origin-angle previous) width)) *max-iso-points*))
                     (reverse-blocked-p (routepoint-position previous) pos)))
        (when path (push (nreverse path) paths))
        (setf path nil))
      (push pos path)
      (setf previous point))
    (when path (push (nreverse path) paths))
    (nreverse paths)))

(defun reverse-direct-step (routing pos time target limit)
  "Shoot the local ground displacement toward TARGET, including wave and current drift."
  (multiple-value-bind (east north) (fine-offset pos target)
    (let* ((bearing (mod (deg (atan east north)) 360d0))
           (heading bearing)
           (distance (fine-distance pos target))
           (dt (min limit 1d0)))
      (loop repeat 14 do
        (multiple-value-bind (sample end)
            (fine-sail-step routing pos time dt (list :heading heading))
          (unless end (return nil))
          (multiple-value-bind (x y) (fine-offset pos end)
            (let* ((length (sqrt (+ (* x x) (* y y))))
                   (next (min limit (* dt (/ distance (max length 1d-10)))))
                   (angle-error (normalize-angle (- bearing (deg (atan x y))))))
              (when (or (< (fine-distance end target) *fine-tune-mark-tolerance*)
                        (and (= dt limit) (>= next limit) (< (abs angle-error) 1d-7)))
                (return (values sample end dt)))
              (setf dt next heading (mod (+ heading angle-error) 360d0)))))))))

(defun reverse-finish-track (routing point first-leg until)
  "Replay one direct or two-board finish from POINT; accept only a physical mark touch."
  (let* ((pos (routepoint-position point)) (time (routepoint-time point))
         (target (routing-dest routing)) (track nil)
         (remaining (or (cdr first-leg) 0d0))
         (board (when first-leg (signum (heading-twa (nth-value 0 (wind-at routing pos time)) (car first-leg)))))
         (last nil))
    (loop repeat *approach-max-iterations* do
      (when (< (fine-distance pos target) *fine-tune-mark-tolerance*)
        (return (if last
                    (let ((end (copy-trackpoint last)))
                      (setf (trackpoint-time end) time (trackpoint-position end) pos (trackpoint-dtf end) 0d0)
                      (append (butlast (route-from-point point)) (nreverse (cons end track))))
                    (route-from-point point))))
      (let* ((switch (routing-forecast-switch-time routing))
             (limit (min 60d0 (timestamp-difference until time)
                         (if (and switch (timestamp< time switch)) (timestamp-difference switch time) 60d0))))
        (when (< limit 1d-7) (return nil))
        (multiple-value-bind (sample end dt)
            (if (> remaining 0.01d0)
                (multiple-value-bind (dir speed) (wind-at routing pos time)
                  (multiple-value-bind (up down) (best-vmg (routing-polars routing) speed)
                    (let* ((toward (abs (heading-twa dir (gate-course-angle pos target))))
                           (angle (if (< toward 90d0) (vmg-twa up) (vmg-twa down))))
                      (multiple-value-bind (s p) (fine-sail-step routing pos time limit (* board angle))
                        (when p
                          (decf remaining (fine-distance pos p))
                          (values s p limit))))))
                (reverse-direct-step routing pos time target limit))
          (unless (and end (plusp dt) (not (reverse-blocked-p pos end))) (return nil))
          ;; The forward search and its tuner consume the signed TWA for each interval.
          (setf (trackpoint-control-mode sample) "twa"
                (trackpoint-control-value sample) (trackpoint-twa sample))
          (push sample track)
          (setf pos end last sample time (routing-time+ time dt)))))))

(defun reverse-best-finish (routing rp)
  "Rank feasible direct and both VMG tack orders from recent forward predecessors."
  (let* ((target (routing-dest routing))
         (until (routing-time+ (routing-starttime routing) (routing-stepmax routing)))
         (best nil))
    (loop for point = rp then (routepoint-predecessor point) while point
          repeat (1+ *approach-lookback*) do
      (let ((pos (routepoint-position point)))
        (multiple-value-bind (twd tws) (wind-at routing pos (routepoint-time point))
          (multiple-value-bind (up down) (best-vmg (routing-polars routing) tws)
            (dolist (first (remove-duplicates
                            (cons nil (approach-first-legs (gate-course-angle pos target)
                                       (course-distance pos target) twd (vmg-twa up) (vmg-twa down)))
                            :test #'equal))
              ;; A rotating wind can move the layline during the approach. Try both a
              ;; nominal board and a longer board that leaves room for a direct finish.
              (dolist (factor (if first '(1d0 1.15d0 1.5d0) '(1d0)))
                (let ((track (reverse-finish-track routing point
                              (when first (cons (car first) (* factor (cdr first)))) until)))
                  (when track (setf best track until (trackpoint-time (car (last track))))))))))))
    best))

(defun reverse-forward-step (routing frontier history start end weather)
  "Continue the same forward search, retaining its absolute radial history."
  (let* ((from (routing-start routing)) (course (gate-course-angle from (routing-dest routing)))
         (left (normalize-heading (- course (routing-fan routing))))
         (right (normalize-heading (+ course (routing-fan routing))))
         (max-dist history) (min-angle (/ 360d0 *max-iso-points*))
         (next-isochrone (make-array *max-iso-points* :initial-element nil)))
    (declare (special max-dist min-angle next-isochrone))
    (destructuring-bind (wind current wave wave-dir) weather
      (loop for rp across frontier do
        (expand-routepoint routing rp left right (timestamp-difference end start) end
                           wind current wave wave-dir (routing-polars routing) min-angle)))
    (filter-isochrone next-isochrone)))

(defun reverse-extend-clock (run routing deadline)
  "Continue past the forward arrival if necessary and clip the last interval to the deadline."
  (loop for p across (reverse-run-frontier run) do (setf (routepoint-predecessor p) nil))
  (loop for time = (reverse-slice-end (first (reverse-run-slices run)))
        while (timestamp< time deadline) do
    (let* ((next (routing-time+ time (funcall (reverse-run-stepper run))))
           (end (if (timestamp< next deadline) next deadline))
           (weather (fine-weather routing time))
           (front (reverse-forward-step routing (reverse-run-frontier run) (reverse-run-history run)
                                        time end weather)))
      (loop for p across front do (setf (routepoint-predecessor p) nil))
      (reverse-save-step run routing time end weather front (reverse-run-history run)
                         (reverse-run-stepper run))))
  (loop while (timestamp>= (reverse-slice-start (first (reverse-run-slices run))) deadline)
        do (pop (reverse-run-slices run)))
  (let ((last (first (reverse-run-slices run))))
    (when (timestamp< deadline (reverse-slice-end last))
      (setf (reverse-slice-end last) deadline (reverse-slice-paths last) nil)))
  ;; A short passage may finish inside the very first step. Compare at mid-passage.
  (when (null (cdr (reverse-run-slices run)))
    (let* ((last (first (reverse-run-slices run))) (start (reverse-slice-start last))
           (mid (routing-time+ start (/ (timestamp-difference deadline start) 2)))
           (origin (create-routepoint nil (routing-start routing) start nil nil))
           (weather (fine-weather routing start))
           (front (reverse-forward-step routing (vector origin) (make-array *max-iso-points* :initial-element 0d0)
                                        start mid weather)))
      (setf (reverse-run-slices run)
            (list (make-reverse-slice :start mid :end deadline :cycle (reverse-slice-cycle last) :cycles (reverse-slice-cycles last))
                  (make-reverse-slice :start start :end mid :cycle (reverse-slice-cycle last)
                                      :cycles (reverse-slice-cycles last) :display t :paths (reverse-forward-paths front))))))
  (setf (reverse-run-frontier run) nil (reverse-run-history run) nil))

(defstruct reverse-candidate position later (radius 0d0 :type double-float))

(defun reverse-front-step (routing front slice weather)
  "Expand with negative elapsed time and weather sampled once per frontier point."
  (let* ((origin (routing-dest routing)) (axis (gate-course-angle origin (routing-start routing)))
         (width (/ 360d0 *max-iso-points*))
         (winners (make-array *max-iso-points* :initial-element nil))
         (time (reverse-slice-start slice)) (dt (timestamp-difference (reverse-slice-end slice) time))
         (count 0) (max-step-distance 0d0))
    (labels ((admit (point)
               (let* ((p (routepoint-position point))
                      (r (routepoint-origin-distance point))
                      (index (mod (floor (routepoint-origin-angle point) width) *max-iso-points*))
                      (old (aref winners index)))
                 (when (or (null old) (> r (reverse-candidate-radius old)))
                   (setf (aref winners index)
                         (make-reverse-candidate :position p
                           :later (routepoint-position (routepoint-predecessor point)) :radius r)))))
             (expand (later)
               ;; Negative elapsed time reverses the complete ground velocity,
               ;; keeping the physical sailing heading for polar/wave evaluation.
               (let* ((leg (copy-routing routing))
                      (*routepoint-sink* #'admit)
                      (left (normalize-heading (- axis 90d0)))
                      (right (normalize-heading (+ axis 90d0))))
                 (setf (routing-start leg) origin (routing-gate leg) nil)
                 (destructuring-bind (wind current wave wave-dir) weather
                   (incf count
                         (expand-routepoint leg (create-routepoint nil later (reverse-slice-end slice) nil nil)
                           left right (- dt) time wind current wave wave-dir (routing-polars routing) width)))))
             (clear-p (candidate)
               (not (reverse-blocked-p (reverse-candidate-position candidate)
                                       (reverse-candidate-later candidate)))))
      ;; Keep the finish available at each earlier boundary, even after an empty step.
      (expand origin)
      (loop for p across front when p do (expand p))
      ;; Match single-leg pruning: retain one winner per bucket, then test land.
      (loop for winner across winners for i from 0 when winner do
        (setf max-step-distance (max max-step-distance
                                    (course-distance (reverse-candidate-position winner)
                                                     (reverse-candidate-later winner))))
        (setf (aref winners i) (when (clear-p winner) (reverse-candidate-position winner))))
      (values winners count max-step-distance))))

(defun reverse-project (origin pos)
  "Azimuthal equidistant coordinates preserve the forward origin's bearing and radius."
  (let ((r (course-distance origin pos)) (a (rad (gate-course-angle origin pos))))
    (complex (* r (cos a)) (* r (sin a)))))

(defun reverse-unproject (origin z)
  "Convert a clipped endpoint back to the sphere, including across the date line."
  (let* ((arc (/ (abs z) cl-geomath::+radius+)) (azimuth (phase z))
         (lat (latlng-latr origin))
         (latitude (asin (max -1d0 (min 1d0 (+ (* (sin lat) (cos arc))
                                               (* (cos lat) (sin arc) (cos azimuth)))))))
         (delta (atan (* (sin azimuth) (sin arc) (cos lat))
                      (- (cos arc) (* (sin lat) (sin latitude))))))
    (make-latlng :lat (deg latitude) :lng (normalize-angle (+ (latlng-lng origin) (deg delta))))))

(defun reverse-cross (a b)
  "Signed planar cross product of two complex coordinate pairs."
  (- (* (realpart a) (imagpart b)) (* (imagpart a) (realpart b))))

(defun reverse-triangle-interval (p q a b)
  "Clip segment P-Q against the forward sector with vertices 0,A,B (counterclockwise)."
  (let ((lo 0d0) (hi 1d0) (d (- q p)))
    ;; An isolated occupied bucket supports just its own ray.
    (when (= a b)
      (let ((den (reverse-cross d a)))
        (when (and (not (zerop a))
                   (<= (abs den) (* +reverse-overlap-tolerance+ (abs a)))
                   (<= (abs (reverse-cross p a)) (* +reverse-overlap-tolerance+ (abs a))))
          (let ((start (realpart (/ p a))) (step (realpart (/ d a))))
            (if (zerop step)
                (when (<= 0 start 1) (return-from reverse-triangle-interval (values 0d0 1d0)))
                (let* ((u (/ (- start) step)) (v (/ (- 1 start) step))
                       (lo (max 0d0 (min u v))) (hi (min 1d0 (max u v))))
                  (when (<= lo hi) (return-from reverse-triangle-interval (values lo hi)))))))
        (unless (zerop den)
          (let* ((f (/ (- (reverse-cross p a)) den))
                 (radius (realpart (/ (+ p (* f d)) a))))
            (when (and (<= 0 f 1) (<= 0 radius 1))
              (return-from reverse-triangle-interval (values f f))))))
      (return-from reverse-triangle-interval nil))
    (when (plusp (reverse-cross a b))
      (loop for u in (list 0 a b) for v in (list a b 0)
            for edge = (- v u) for h = (reverse-cross edge (- p u))
            for slope = (reverse-cross edge d) do
        (cond ((plusp slope) (setf lo (max lo (/ (- h) slope))))
              ((minusp slope) (setf hi (min hi (/ (- h) slope))))
              ((minusp h) (return-from reverse-triangle-interval nil)))
        (when (> lo hi) (return-from reverse-triangle-interval nil)))
      (values lo hi))))

(defun reverse-boundary (origin pos)
  "Allow coordinate roundoff at an equal-time tangency."
  (let* ((z (reverse-project origin pos)) (r (abs z)))
    (if (plusp r) (* z (+ 1d0 (/ +reverse-overlap-tolerance+ r))) z)))

(defun reverse-sectors (origin paths)
  "Index each observed forward edge by its two adjacent absolute bearing buckets."
  (let ((sectors (make-array *max-iso-points* :initial-element nil))
        (width (/ 360d0 *max-iso-points*)))
    (dolist (path paths)
      (unless (cdr path)
        (let* ((z (reverse-boundary origin (first path)))
               (index (mod (floor (mod (deg (phase z)) 360d0) width) *max-iso-points*)))
          (push (cons z z) (aref sectors index))))
      (loop for (a b) on path while b
            for za = (reverse-boundary origin a) for zb = (reverse-boundary origin b)
            for index = (mod (floor (mod (deg (phase za)) 360d0) width) *max-iso-points*)
            for edge = (cons za zb) do
        (push edge (aref sectors index))
        (push edge (aref sectors (mod (1+ index) *max-iso-points*)))))
    sectors))

(defun reverse-overlap-edge (origin sectors p q)
  "Intersect only forward sectors swept by this reverse edge. Merge adjacent clipped intervals."
  (let* ((a (reverse-project origin p)) (b (reverse-project origin q))
         (width (/ 360d0 *max-iso-points*))
         (begin (mod (deg (phase a)) 360d0))
         (end (+ begin (normalize-angle (- (deg (phase b)) begin))))
         (intervals nil) (previous nil))
    (loop for i from (floor (min begin end) width) to (floor (max begin end) width) do
      (dolist (edge (aref sectors (mod i *max-iso-points*)))
        (unless (eq edge previous)
          (multiple-value-bind (lo hi) (reverse-triangle-interval a b (car edge) (cdr edge))
            (when lo (push (cons lo hi) intervals))))
        (setf previous edge)))
    (let ((merged nil))
      (dolist (pair (sort intervals #'< :key #'car))
        (if (and merged (<= (car pair) (+ (cdar merged) 1d-10)))
            (setf (cdar merged) (max (cdar merged) (cdr pair)))
            (push pair merged)))
      (loop for (lo . hi) in (nreverse merged)
            collect (list (if (zerop lo) p (reverse-unproject origin (+ a (* lo (- b a)))))
                          (if (= hi 1d0) q (reverse-unproject origin (+ a (* hi (- b a))))))))))

(defun reverse-point-overlaps-p (origin sectors pos)
  "Check an isolated reverse sample at its actual bearing, allowing inverse-step roundoff."
  (let* ((p (reverse-project origin pos))
         (i (mod (floor (mod (deg (phase p)) 360d0) (/ 360d0 *max-iso-points*)) *max-iso-points*)))
    (some (lambda (edge)
            (if (= (car edge) (cdr edge))
                (and (<= (abs (reverse-cross p (car edge))) (* 0.1d0 (abs (car edge))))
                     (<= (abs p) (abs (car edge))))
                (reverse-triangle-interval p p (car edge) (cdr edge))))
          (aref sectors i))))

(defun reverse-display-paths (routing front forward slice &optional max-step-distance)
  "Build separate reverse and overlap lines; preserve bucket, coast and clipping gaps."
  (let* ((origin (routing-start routing)) (dest (routing-dest routing))
         (width (/ 360d0 *max-iso-points*))
         (first (floor (mod (- (gate-course-angle dest origin) 90d0) 360d0) width))
         (sectors (reverse-sectors origin forward))
         (paths nil) (overlap nil) (previous nil) (path nil) (highlight nil))
    (labels ((save-line (points)
               (make-isochrone :center dest :time (reverse-slice-start slice)
                 :wind-time (reverse-slice-start slice) :cycle (reverse-slice-cycle slice)
                 :forecast-model (routing-model-at routing (reverse-slice-start slice))
                 :path (nreverse points)))
             (flush ()
               (when (and path (null (cdr path)) (reverse-point-overlaps-p origin sectors (first path)))
                 (push (save-line (copy-list path)) overlap))
               (when path (push (save-line path) paths))
               (when highlight (push (save-line highlight) overlap))
               (setf path nil highlight nil)))
      (loop for i below (length front) for p = (aref front (mod (+ first i) (length front))) do
        (cond ((null p) (flush))
              ((and previous
                    ;; Adjacent bearing buckets can belong to separate arrival fronts.
                    ;; Never draw a radial spoke between them, or clip that spoke as overlap.
                    (or (null max-step-distance)
                        (<= (course-distance previous p)
                            (max (* 4 max-step-distance)
                                 (* 4 (rad width) (max (course-distance dest previous)
                                                       (course-distance dest p))))))
                    (not (reverse-blocked-p previous p)))
               (push p path)
               (dolist (part (reverse-overlap-edge origin sectors previous p))
                 (unless (and highlight (eq (first highlight) (first part)))
                   (when highlight (push (save-line highlight) overlap))
                   (setf highlight (list (first part))))
                 (unless (< (fine-distance (first part) (second part)) 0.001d0)
                   (push (second part) highlight))))
              (t (flush) (push p path)))
        (setf previous p))
      (flush))
    (values paths overlap)))

(defun reverse-overlay (routing run report)
  "Walk the recorded UTC clock in reverse. Preserve completed snapshots on a coverage failure."
  (reverse-extend-clock run routing (reverse-result-deadline report))
  (let ((front #()) (lines nil) (overlap nil) (expanded nil))
    (unwind-protect
         (loop for (slice earlier) on (reverse-run-slices run)
               for time = (reverse-slice-start slice)
               for weather = (handler-case (reverse-weather routing time (reverse-slice-cycles slice))
                               (error (e)
                                 (log2:warning "Reverse forecast unavailable at ~a: ~a" time e)
                                 (setf (reverse-result-status report) "forecast-unavailable"
                                       (reverse-result-incomplete-at report) time)
                                 (return))) do
           (multiple-value-bind (next count max-step-distance) (reverse-front-step routing front slice weather)
             (setf front next)
             (incf (reverse-result-steps report))
             (incf (reverse-result-candidates report) count)
             ;; An adverse tide can close the finish temporarily. Earlier seeds still matter.
             (when (find-if #'identity front) (setf expanded t))
             (when (and earlier (reverse-slice-display earlier))
               (multiple-value-bind (paths parts)
                   (reverse-display-paths routing front (reverse-slice-paths earlier) slice max-step-distance)
                 (setf lines (nconc paths lines) overlap (nconc parts overlap)))))
           finally (setf (reverse-result-status report) (if expanded "complete" "empty-frontier")))
      (setf (reverse-result-isochrones report) (coerce lines 'vector)
            (reverse-result-overlap report) (coerce overlap 'vector)))))

(defun reverse-comparison-fronts (routing run)
  "Publish the actual forward comparison geometry, including sub-hour and post-arrival samples."
  (loop for slice in (reverse-run-slices run) append
    (loop for path in (reverse-slice-paths slice)
          collect (make-isochrone :center (routing-start routing) :time (reverse-slice-end slice)
                    :cycle (reverse-slice-cycle slice) :wind-time (reverse-slice-start slice)
                    :forecast-model (routing-model-at routing (reverse-slice-start slice))
                    :offset (floor (timestamp-difference (reverse-slice-end slice) (reverse-slice-cycle slice)) 3600)
                    :path path))))

(defun route-with-reverse-search (routing)
  "Compute and preserve the forward result before attempting the optional deadline analysis."
  (when (or (routing-waypoints routing) (routing-gate routing))
    (error "Reverse routing requires a route without waypoints or gates"))
  (let* ((run (make-reverse-run)) (percent (routing-reverse-percent routing))
         (report (make-reverse-result :status "forward-unreached" :percent percent))
         (*fine-tune-weather* (make-hash-table :test #'equal)) (*fine-tune-weather-cache-limit* 8))
    (multiple-value-bind (info best)
        (get-route-leg routing (lambda (start end weather frontier history stepper)
                                 (reverse-save-step run routing start end weather frontier history stepper)))
      (when (string= (routeinfo-status info) "reached")
        (setf (reverse-result-status report) "exact-finish-unavailable")
        (handler-case
            (let ((track (reverse-best-finish routing best)))
              (when track
                (setf (routeinfo-best info) track
                      (reverse-result-baseline report) (trackpoint-time (car (last track))))))
          (error (e) (log2:warning "Exact finish unavailable: ~a" e))))
      (let ((*fine-tune-endpoint-tolerance* (if (reverse-result-baseline report)
                                                (max 0d0 (- *fine-tune-mark-tolerance*
                                                  (fine-distance (trackpoint-position (car (last (routeinfo-best info))))
                                                                 (routing-dest routing))))
                                                *fine-tune-endpoint-tolerance*)))
        (fine-tune-route routing info))
      (setf (routeinfo-reverse-routing info) report)
      (when (reverse-result-baseline report)
        (let* ((eta (trackpoint-time (car (last (routeinfo-best info)))))
               (seconds (timestamp-difference eta (routing-starttime routing)))
               (horizon (* 3600 *max-route-hours*)))
          (setf (reverse-result-baseline report) eta)
          (cond ((or (<= seconds 0) (> percent (* 100 (- (/ horizon seconds) 1))))
                 (setf (reverse-result-status report) "deadline-outside-horizon"))
                (t (setf (reverse-result-deadline report) (routing-time+ eta (* seconds (/ percent 100d0))))
                   (handler-case (progn (reverse-overlay routing run report)
                                        (setf (routeinfo-isochrones info) (reverse-comparison-fronts routing run)))
                     (error (e) (log2:warning "Reverse analysis stopped: ~a" e)
                                (setf (reverse-result-status report) "coverage-unavailable")))))))
      info)))
