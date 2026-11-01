;;; Load after Bitsailor and gate-search-tests.cl; call TEST-GATE-ARTIFICIAL-ORIGIN.
(in-package :bitsailor)

;;; Check common-origin geometry, bounded resolution and fractional turn-back paths
;;; against real constant-wind polars. No map or forecast downloads are needed.
(defun test-gate-artificial-origin ()
  (let* ((time (parse-datetime "2026-11-01T00:00:00Z"))
         (mid (make-latlng :lat 38d0 :lng 2d0))
         (gate (make-gate :a (add-distance-exact mid 10000d0 0d0)
                          :b (add-distance-exact mid 10000d0 180d0)))
         (target (add-distance-exact mid 100000d0 90d0))
         (targets (vector gate target))
         (start (add-distance-exact mid 30000d0 270d0)))
    (multiple-value-bind (centers frames) (gate-stage-geometry start targets)
      (let* ((*gate-bucket-frames* frames)
             (origin (aref centers 1))
             (width (course-distance (gate-a gate) (gate-b gate)))
             (distance (course-distance mid origin))
             (rp (create-routepoint nil mid time nil nil)))
        (assert (eq start (aref centers 0)))
        (assert (< (abs (- distance (* 2d0 width))) 1d-4))
        (assert (< (abs (normalize-angle (- (course-angle mid origin)
                                             (course-angle mid target) 180d0))) 1d-6))
        ;; An endpoint's bucket/radius must not depend on which actual crossing
        ;; origin reached it. Its steering origin remains distinct.
        (let* ((point (create-routepoint rp (add-distance-exact mid 1000d0 90d0) time nil nil))
               (a (make-gate-state :stage 1 :point point :origin rp))
               (other (create-routepoint nil (gate-a gate) time nil nil))
               (b (make-gate-state :stage 1 :point point :origin other)))
          (assert (equal (gate-state-key a targets centers) (gate-state-key b targets centers))))
        ;; Approaching the next target from a lower endpoint can shrink midpoint
        ;; radius while increasing artificial-origin radius.
        (let* ((from (gate-b gate))
               (to (add-distance-exact from 1000d0 (course-angle from target))))
          (assert (< (course-distance mid to) (course-distance mid from)))
          (assert (> (course-distance origin to) (course-distance origin from))))
        ;; The nonlinear full-circle map preserves the fixed bucket budget and
        ;; separates the gate endpoints even if the origin is made more distant.
        (let ((keys (make-hash-table :test #'equal)))
          (loop :for angle :from 0d0 :below 360d0 :by 0.1d0 :do
            (let* ((point (create-routepoint rp (add-distance-exact origin 50000d0 angle) time nil nil))
                   (state (make-gate-state :stage 1 :origin rp :point point)))
              (setf (gethash (gate-state-key state targets centers) keys) t)))
          (assert (<= (hash-table-count keys) *max-iso-points*)))
        (assert (> (gate-bucket-frame-scale (aref frames 1)) 1d0))))
    ;; Clamp factors smaller than two, and keep degenerate gates finite.
    (let ((*gate-origin-distance-factor* 0.5d0))
      (assert (< (abs (- (course-distance mid (gate-artificial-origin gate target))
                        (* 2d0 (course-distance (gate-a gate) (gate-b gate))))) 1d-4)))
    (let ((tiny (make-gate :a mid :b mid)))
      (assert (< (abs (- (course-distance mid (gate-artificial-origin tiny target)) 1000d0)) 1d-4)))
    ;; Longitude wrapping must not send the artificial origin to the wrong side.
    (let* ((dateline (make-gate :a (make-latlng :lat 0.1d0 :lng 179.9d0)
                               :b (make-latlng :lat -0.1d0 :lng 179.9d0)))
           (east (make-latlng :lat 0d0 :lng -179.8d0))
           (origin (gate-artificial-origin dateline east)))
      (assert (< (abs (normalize-angle (- (course-angle (target-position dateline) origin) 270d0))) 0.1d0))))
  (let* ((start (parse-datetime "2026-11-01T00:00:00Z"))
         (pos (make-latlng :lat 0d0 :lng 0d0))
         (grid (cl-weather::make-gribinfo :lat-start 1d0 :lat-end -1d0 :lat-points 3
                 :lon-start 0d0 :lon-end 360d0 :lon-points 361 :i-inc 1d0 :j-inc 1d0))
         (field (cl-weather::make-uv :cycle start :info grid
                  :u-array (make-array 1083 :element-type 'double-float :initial-element 0d0)
                  :v-array (make-array 1083 :element-type 'double-float :initial-element -10d0)))
         (wind (cl-weather::make-params :fc0 field :fc1 field))
         (polars (make-cpolars :label "constant"
                   :maxspeed (make-array '(1801 101) :initial-element '(5d0 0))
                   :vmg (make-array 101 :initial-element
                          (list (make-vmg :twa 45d0) (make-vmg :twa 135d0)))))
         (routing (make-routing :polars polars))
         (rp (create-routepoint nil pos start nil nil))
         (end-time (adjust-timestamp start (:offset :sec 600)))
         (point (create-routepoint rp (add-distance-exact pos 3000d0 90d0) end-time
                                  90d0 90d0 nil 5d0 0 0d0 10d0 0d0 0d0))
         (gate-mid (add-distance-exact pos 1000d0 90d0))
         (gate (make-gate :a (add-distance-exact gate-mid 500d0 0d0)
                          :b (add-distance-exact gate-mid 500d0 180d0)))
         (second-mid (add-distance-exact pos 500d0 270d0))
         (second-gate (make-gate :a (add-distance-exact second-mid 500d0 0d0)
                                 :b (add-distance-exact second-mid 500d0 180d0)))
         (destination (add-distance-exact pos 3000d0 270d0))
         (state (advance-gate-state (make-gate-state :point rp :origin rp) point
                                   (vector gate second-gate destination) routing wind nil nil nil))
         (endpoint (gate-state-point state))
         (crossing2 (routepoint-predecessor endpoint))
         (crossing1 (routepoint-predecessor crossing2)))
    ;; Touch the first gate east of the start, reverse in the remaining time,
    ;; and touch the second gate west of the start, all on the same clock.
    (assert (= (gate-state-stage state) 2))
    (assert (timestamp= (routepoint-time endpoint) end-time))
    (assert (< (abs (- (timestamp-difference (routepoint-time crossing1) start) 200d0)) 0.1d0))
    (assert (< (abs (- (timestamp-difference (routepoint-time crossing2) start) 500d0)) 0.2d0))
    (assert (< (abs (- (course-distance pos (routepoint-position endpoint)) 1000d0)) 1d0))
    (assert (< (latlng-lng (routepoint-position endpoint)) 0d0))
    (assert (gate-state-in-fan-p state (vector gate second-gate destination) 120d0))
    ;; Check bent segments, not the source-to-endpoint chord or discarded overshoot.
    (call-with-gate-test-functions
     (list (cons 'approach-blocked-p
                 (lambda (from to) (declare (ignore to))
                   (> (latlng-lng from) (* 0.9d0 (latlng-lng gate-mid))))))
     (lambda () (assert (gate-state-blocked-p state))))
    (call-with-gate-test-functions
     (list (cons 'approach-blocked-p
                 (lambda (from to)
                   (or (> (course-distance pos from) 1200d0)
                       (> (course-distance pos to) 1200d0)))))
     (lambda () (assert (not (gate-state-blocked-p state)))))
    ;; If the new target is dead upwind, select one of the VMG-limit headings.
    (let* ((north (add-distance-exact gate-mid 10000d0 0d0))
           (turn (gate-turn-endpoint routing crossing1 end-time north wind nil nil nil)))
      (assert (= (abs (routepoint-twa turn)) 45d0))
      (assert (timestamp= (routepoint-time turn) end-time)))
    ;; The turned remainder must include the same wave and current adjustments
    ;; as ordinary expansion. A 4 m following sea adds 10%; a westward 1 m/s
    ;; current then adds to the westbound boat velocity.
    (let* ((turn-time (adjust-timestamp (routepoint-time crossing1) (:offset :sec 100)))
           (flow (cl-weather::make-uv :cycle start :info grid
                   :u-array (make-array 1083 :element-type 'double-float :initial-element -1d0)
                   :v-array (make-array 1083 :element-type 'double-float :initial-element 0d0)))
           (current (cl-weather::make-params :fc0 flow :fc1 flow))
           (height (cl-weather::make-gribmessage :info grid
                     :data (make-array 1083 :element-type 'double-float :initial-element 4d0)))
           (direction (cl-weather::make-gribmessage :info grid
                        :data (make-array 1083 :element-type 'double-float :initial-element 90d0)))
           (turn (gate-turn-endpoint routing crossing1 turn-time destination wind current
                   (cl-weather::make-params :fc0 height :fc1 height)
                   (cl-weather::make-params :fc0 direction :fc1 direction))))
      (assert (< (abs (- (routepoint-speed turn) 5.5d0)) 1d-8))
      (assert (< (abs (- (course-distance (routepoint-position crossing1)
                                          (routepoint-position turn)) 650d0)) 1d0)))
    ;; Exercise the full search, not just the crossing helper: approach a gate
    ;; east of the start, touch it, then reach a destination west of the start.
    (setf (routing-start routing) pos (routing-dest routing) destination
          (routing-starttime routing) start (routing-stepmax routing) 1800
          (routing-twa-angles routing)
          (make-array 5 :element-type 'double-float
                        :initial-contents '(45d0 90d0 135d0 45d0 135d0)))
    (call-with-gate-test-functions
     (list (cons 'get-params (lambda (&rest args) (declare (ignore args)) wind))
           (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) nil)))
     (lambda ()
       (let* ((info (get-route-through-gates routing (list gate)))
              (track (routeinfo-best info)))
         (assert (string= (routeinfo-status info) "reached"))
         (assert (some (lambda (p) (> (latlng-lng (trackpoint-position p))
                                      (* 0.99d0 (latlng-lng gate-mid)))) track))
         (assert (< (latlng-lng (trackpoint-position (car (last track)))) 0d0))
         ;; Exact collinear extension leaves the direct crossing feasible.
         ;; In constant wind, this solvable case must not become slower.
         (let* ((wide (make-gate :a (add-distance-exact gate-mid 1000d0 0d0)
                                :b (add-distance-exact gate-mid 1000d0 180d0)))
                (long-info (get-route-through-gates routing (list wide))))
           (assert (string= (routeinfo-status long-info) "reached"))
           (assert (timestamp<= (trackpoint-time (car (last (routeinfo-best long-info))))
                                (trackpoint-time (car (last track))))))))))
  (format t "~&Artificial-origin geometry, bounded buckets and fractional gate-turn regressions passed.~%"))
