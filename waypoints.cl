;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multi-route: route through intermediate targets: points that are hit exactly
;;; and gates that are crossed (see GATE-CROSSING in filter.cl).
;;;
;;; Consecutive gates and the following point form one shared-clock search
;;; (gate-search.cl). Point waypoints divide the route into sequential legs.
;;; At each intermediate point, try direct or VMG approaches from the selected
;;; frontier point and its recent predecessors, without going back before the
;;; last gate crossing. The earliest successful approach starts the next leg.
;;; If no approach succeeds, stop with the track reached so far. The final
;;; destination uses the search engine's arrival criterion without this refinement.

(in-package :bitsailor)

(defparameter *approach-max-step* 300
  "Maximum time step in seconds when sailing the approach to an intermediate target")

(defparameter *approach-max-iterations* 1000)

;;; Return wind direction, speed and the actual forecast parameters at POS/TIME.
(defun wind-at (routing pos time)
  (let ((params (routing-wind-params routing time)))
    (multiple-value-bind (direction speed)
        (interpolate-uv (latlng-lat pos) (latlng-lng pos) params)
      (values direction speed params))))

(defun approach-first-legs (bearing distance twd up-twa down-twa)
  "Candidate first legs (heading . length) of a tack/gybe from a point to a target
at BEARING and DISTANCE. Returns (NIL) if the target is within the VMG angles
(direct approach) and NIL if no solution exists."
  (let ((abs-twa (abs (heading-twa twd bearing))))
    (if (<= up-twa abs-twa down-twa)
        (list nil)
        (let* ((vmg-twa (if (< abs-twa up-twa) up-twa down-twa))
               (h1 (normalize-heading (+ twd vmg-twa)))
               (h2 (normalize-heading (- twd vmg-twa)))
               (det (sin (rad (- h1 h2)))))
          (when (> (abs det) 1d-6)
            ;; Solve l1*u(h1) + l2*u(h2) = distance*u(bearing) in the local plane
            (let ((l1 (/ (* distance (sin (rad (- bearing h2)))) det))
                  (l2 (/ (* distance (sin (rad (- h1 bearing)))) det)))
              (when (and (>= l1 0d0) (>= l2 0d0))
                (list (cons h1 l1) (cons h2 l2)))))))))

;;; Test an approach segment for land in vector mode, or just its endpoint in bitmap mode.
(defun approach-blocked-p (from to)
  (if *use-bitmap*
      (bm-is-land to)
      (intersects-land-p from to)))

(defun sail-approach (routing rp target first-leg)
  "Sail from routepoint RP to TARGET, first along FIRST-LEG (heading . length) if
given, then directly. Returns the routepoint at TARGET, or NIL if the path is
blocked by land, progress is too slow, or the iteration limit is reached.
Wind is resampled each step; currents and wave effects are not applied here."
  (let ((polars (routing-polars routing))
        (current rp)
        (fixed-heading (car first-leg))
        (leg-remaining (or (cdr first-leg) 0d0)))
    (dotimes (i *approach-max-iterations* nil)
      (declare (ignorable i))
      (let* ((pos (routepoint-position current))
             (time (routepoint-time current))
             (to-target (course-distance pos target)))
        (multiple-value-bind (twd tws) (wind-at routing pos time)
          (let* ((on-first-leg (and fixed-heading (> leg-remaining 1d0)))
                 (heading (if on-first-leg
                              fixed-heading
                              (normalize-heading (course-angle pos target))))
                 (twa (heading-twa twd heading)))
            (multiple-value-bind (speed sail) (twa-boatspeed polars tws twa)
              (when (< speed 0.05d0)
                (return nil))
              (let* ((reach (if on-first-leg leg-remaining to-target))
                     (final (and (not on-first-leg)
                                 (<= to-target (* speed *approach-max-step*))))
                     (distance (min reach (* speed *approach-max-step*)))
                     (dt (/ distance speed))
                     (new-pos (if final
                                  target
                                  (add-distance-estimate pos distance heading))))
                (when (approach-blocked-p pos new-pos)
                  (return nil))
                (when on-first-leg
                  (decf leg-remaining distance))
                (setf current
                      (create-routepoint current new-pos
                                         (adjust-timestamp time (:offset :nsec (round (* dt 1d9))))
                                         twa heading
                                         (course-distance new-pos target)
                                         speed sail twd tws 0d0 0d0))
                (when final
                  (return current))))))))))

(defun approach-target (routing rp target)
  "Try a direct or VMG tack/gybe approach from RP to TARGET. Return the first
feasible candidate's final routepoint, or NIL; tack orders are not ranked by arrival time."
  (let* ((pos (routepoint-position rp))
         (distance (course-distance pos target)))
    (multiple-value-bind (twd tws) (wind-at routing pos (routepoint-time rp))
      (multiple-value-bind (up down) (best-vmg (routing-polars routing) tws)
        (let ((candidates
                (approach-first-legs (normalize-heading (course-angle pos target))
                                     distance twd (vmg-twa up) (vmg-twa down))))
          (loop
            :for first-leg :in candidates
            :for result = (sail-approach routing rp target first-leg)
            :when result :return result))))))

(defparameter *approach-lookback* 12
  "Number of predecessors of the last isochrone point that are tried as approach start points")

(defparameter *gate-samples* 10
  "Number of intervals along a gate that are tried as crossing points")

;;; Generate equally spaced great-circle samples along the gate, including both endpoints; used by
;;; the legacy sampled approach helper.
(defun gate-sample-points (gate)
  (let ((a (gate-a gate)) (b (gate-b gate)))
    (let ((len (course-distance a b)) (angle (course-angle a b)))
      (loop :for i :from 0 :to *gate-samples*
            :collect (add-distance-exact a (* len (/ i *gate-samples*)) angle)))))

(defun best-approach (routing rp gate target &optional earliest-start)
  "Return the earliest successful approach tried from RP and up to
*APPROACH-LOOKBACK* predecessors, never starting before EARLIEST-START when supplied.
TARGET is the point waypoint. The legacy GATE mode samples points along the gate
and treats RP as an existing arrival; shared-clock gate routing does not use it.
Return NIL if no approach succeeds and no existing gate arrival is supplied."
  (let ((best (when gate rp))
        (targets (if gate (gate-sample-points gate) (list target))))
    (loop :for start = rp :then (routepoint-predecessor start)
          :for i :from 0 :to *approach-lookback*
          :while (and start (or (null earliest-start)
                                (timestamp<= earliest-start (routepoint-time start))))
          :do (dolist (tg targets)
                (let ((arrival (approach-target routing start tg)))
                  (when (and arrival
                             (or (null best)
                                 (timestamp< (routepoint-time arrival) (routepoint-time best))))
                    (setf best arrival)))))
    best))

(defun target-position (target)
  "Position that a leg towards TARGET (a latlng or a GATE) is aimed at."
  (if (gate-p target)
      (let* ((a (gate-a target)) (b (gate-b target))
             (distance (course-distance a b)))
        ;; A degenerate gate has a position but no defined endpoint bearing.
        (if (< distance 1d-3) a
            (add-distance-exact a (/ distance 2d0) (course-angle a b))))
      target))

;;; Route consecutive gate groups between point waypoints, concatenate results and stop on failure
;;; or duration exhaustion.
(defun get-route-via-waypoints (routing)
  (let* ((targets (group-gate-targets routing))
         (total (routing-stepmax routing))
         (start-time (parse-datetime-local (routing-starttime routing)))
         (elapsed0 (now))
         (pos (routing-start routing))
         (time start-time)
         (status "reached")
         (track nil)
         (isochrones nil)
         (tracks nil)
         (steps 0)
         (points 0)
         (info nil)
         (initial-cycle nil))
    (loop
      :for (group . more) :on targets
      :for gates = (butlast group)
      :for target = (car (last group))
      :for gate = nil
      :for remaining = (- total (round (timestamp-difference time start-time)))
      :do (when (<= remaining 0)
            (setf status "max_duration")
            (return))
          (let ((leg (copy-routing routing)))
            (setf (routing-start leg) pos
                  (routing-dest leg) target
                  (routing-starttime leg) time
                  (routing-stepmax leg) remaining
                  (routing-box leg) (make-routing-box pos target)
                  (routing-%dist leg) nil
                  (routing-gate leg) gate
                  (routing-waypoints leg) nil)
            (multiple-value-bind (leg-info best-rp last-crossing) (if gates (get-route-through-gates leg gates) (get-route-leg leg))
              (let ((leg-track (routeinfo-best leg-info)))
                (unless info (setf initial-cycle (routeinfo-initial-cycle leg-info)))
                (setf info leg-info)
                (incf steps (routestats-steps (routeinfo-stats leg-info)))
                (incf points (routestats-points (routeinfo-stats leg-info)))
                (setf isochrones (append isochrones (routeinfo-isochrones leg-info))
                      tracks (append tracks (routeinfo-tracks leg-info)))
                (cond
                  ((string/= (routeinfo-status leg-info) "reached")
                   (setf status (routeinfo-status leg-info)
                         track (append (butlast track) leg-track))
                   (return))
                  ((null more)
                   (setf track (append (butlast track) leg-track))
                   (return))
                  (t
                   ;; Refine the approach to this intermediate point, trying recent
                   ;; predecessors no earlier than the last gate crossing.
                   (let ((arrival (best-approach leg best-rp gate target
                                                 (when last-crossing (routepoint-time last-crossing)))))
                     (cond
                       (arrival
                        (setf track (append (butlast track) (route-from-point arrival))
                              pos (routepoint-position arrival)
                              time (routepoint-time arrival)))
                       (t
                        (log2:warning "Approach to waypoint ~a failed, routing stopped"
                                      (format-latlng nil target))
                        (setf status "no_route"
                              track (append (butlast track) leg-track))
                        (return))))))))))
    (make-routeinfo :forecast-model (routing-forecast-model routing)
                    :requested-cycle (when (routing-cycle routing) (cl-weather::cycle-timestamp (routing-cycle routing)))
                    :initial-cycle initial-cycle :status status
                    :best track
                    :stats (get-statistics track (timestamp-difference (now) elapsed0) steps points)
                    :polars (routeinfo-polars info)
                    :options (routing-options routing)
                    :tracks tracks
                    :isochrones isochrones)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
