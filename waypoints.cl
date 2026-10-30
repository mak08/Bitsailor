;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multi-route: route through intermediate targets: points that are hit exactly
;;; and gates that are crossed (see GATE-CROSSING in filter.cl).
;;;
;;; Each intermediate leg runs isochrones until the next isochrone would lie
;;; beyond the target. From the closest point of that isochrone a VMG path to
;;; the target is sailed: one leg if the target is within the VMG angles,
;;; otherwise a tack/gybe point is computed. The approach is also tried from
;;; the predecessors of that point and the earliest arrival wins, so that the
;;; last (non-VMG) isochrone step is shortened. The arrival time becomes the start
;;; time of the next leg. If the approach is not feasible (land), routing stops
;;; at the current isochrone.

(in-package :bitsailor)

(defparameter *approach-max-step* 300
  "Maximum time step in seconds when sailing the approach to an intermediate target")

(defparameter *approach-max-iterations* 1000)

(defun wind-at (routing pos time)
  (let ((params (get-params (or (routing-forecast-model routing)
                                'cl-weather:noaa-gfs-wind)
                            time)))
    (interpolate-uv (latlng-lat pos) (latlng-lng pos) params)))

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

(defun approach-blocked-p (from to)
  (if *use-bitmap*
      (bm-is-land to)
      (intersects-land-p from to)))

(defun sail-approach (routing rp target first-leg)
  "Sail from routepoint RP to TARGET, first along FIRST-LEG (heading . length) if
given, then directly. Returns the routepoint at TARGET, or NIL if the path is
blocked by land or the boat cannot make progress."
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
  "Sail a VMG path from the routepoint RP to TARGET. Returns the final routepoint or NIL."
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

(defun gate-sample-points (gate)
  (let ((a (gate-a gate)) (b (gate-b gate)))
    (let ((len (course-distance a b)) (angle (course-angle a b)))
      (loop :for i :from 0 :to *gate-samples*
            :collect (add-distance-exact a (* len (/ i *gate-samples*)) angle)))))

(defun best-approach (routing rp gate target)
  "Earliest arrival at TARGET (a point) or at any sampled point of GATE, found by
sailing the VMG approach from RP and from its first *APPROACH-LOOKBACK* predecessors.
The last isochrone step is not optimal VMG, so starting earlier on the track
shortens that step. For a gate RP itself is a valid arrival. Returns a routepoint or NIL."
  (let ((best (when gate rp))
        (targets (if gate (gate-sample-points gate) (list target))))
    (loop :for start = rp :then (routepoint-predecessor start)
          :for i :from 0 :to *approach-lookback*
          :while start
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
      (let ((a (gate-a target)) (b (gate-b target)))
        (add-distance-exact a (/ (course-distance a b) 2d0) (course-angle a b)))
      target))

(defun get-route-via-waypoints (routing)
  (let* ((targets (append (routing-waypoints routing) (list (routing-dest routing))))
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
         (info nil))
    (loop
      :for (item . more) :on targets
      :for gate = (when (gate-p item)
                    (make-gate :a (gate-a item) :b (gate-b item)
                               :next (target-position (first more))))
      :for target = (target-position item)
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
            (multiple-value-bind (leg-info best-rp) (get-route-leg leg)
              (let ((leg-track (routeinfo-best leg-info)))
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
                   ;; Sail the final part to the point or gate on optimal VMG,
                   ;; starting from the best of the last few track points
                   (let ((arrival (best-approach leg best-rp gate target)))
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
    (make-routeinfo :status status
                    :best track
                    :stats (get-statistics track (timestamp-difference (now) elapsed0) steps points)
                    :polars (routeinfo-polars info)
                    :options (routing-options routing)
                    :tracks tracks
                    :isochrones isochrones)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
