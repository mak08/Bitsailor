;;; Load after (asdf:load-system "bitsailor"), then call TEST-GATE-SEARCH,
;;; TEST-GATE-SEARCH-EXPANSION, TEST-GATE-FRONTIER-PRUNING,
;;; TEST-GATE-RETIREMENT-AND-PROGRESS and TEST-GATE-RESULT-SELECTION.
;;; Fixtures require no map or weather downloads.
(in-package :bitsailor)

(defun call-with-gate-test-functions (bindings thunk)
  (let ((originals (mapcar (lambda (binding)
                             (cons (car binding) (symbol-function (car binding)))) bindings)))
    (unwind-protect
         (progn (dolist (binding bindings) (setf (symbol-function (car binding)) (cdr binding)))
                (funcall thunk))
      (dolist (binding originals) (setf (symbol-function (car binding)) (cdr binding))))))

(defun test-gate-search ()
  (let* ((start (parse-datetime "2026-10-30T00:00:00Z"))
         (gate (make-gate :a (make-latlng :lat -1d0 :lng 1d0)
                          :b (make-latlng :lat 2d0 :lng 1d0)))
         (destination (make-latlng :lat 1d0 :lng 2d0))
         (routing (make-routing :start (make-latlng :lat 0d0 :lng 0d0)
                    :dest destination :starttime start :stepmax 2400
                    :polars (make-cpolars :label "test")))
         (saw-early nil) (saw-late nil) (clocks (make-hash-table)))
    ;; Script two spatial alternatives, keeping the real search, pruning and
    ;; crossing logic. The early crossing is slow downstream. The late crossing
    ;; occurs several steps later and reaches the destination first.
    (call-with-gate-test-functions
     (list
      (cons 'wind-at (lambda (&rest args) (declare (ignore args))
                       (values 0d0 10d0 (cl-weather::make-params :fc1 (cl-weather::make-uv :cycle start)))))
      (cons 'get-params (lambda (&rest args) (declare (ignore args))
                          (cl-weather::make-params :fc1 (cl-weather::make-uv :cycle start))))
      (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) nil))
      (cons 'expand-routepoint
            (lambda (leg rp left right dt time wind current wave wave-dir polars delta)
              (declare (ignore left right dt wind current wave wave-dir polars delta))
              (let* ((pos (routepoint-position rp)) (lat (latlng-lat pos)) (lon (latlng-lng pos))
                     (seconds (round (timestamp-difference time start)))
                     (stage (if (> (latlng-lng (routing-start leg)) 0d0) 1 0)))
                (pushnew stage (gethash seconds clocks))
                (when (and (= lat 0d0) (= stage 1)) (setf saw-early t))
                (when (and (= lat 1d0) (= stage 1)) (setf saw-late t))
                (flet ((emit (lat lon)
                         (funcall *routepoint-sink*
                           (create-routepoint rp (make-latlng :lat lat :lng lon)
                                              time 90d0 90d0 nil 10d0 0 0d0 10d0))))
                  (cond ((null (routepoint-predecessor rp)) (emit 0d0 1.1d0) (emit 1d0 0.1d0))
                        ((= lat 0d0) (emit lat (+ lon 0.05d0)))
                        ((< lon 1d0) (emit lat (+ lon 0.3d0)))
                        (t (emit lat 2.2d0))))
                2))))
     (lambda ()
       (multiple-value-bind (info rp) (get-route-through-gates routing (list gate))
         (assert (string= (routeinfo-status info) "reached"))
         (assert (= (latlng-lat (routepoint-position rp)) 1d0))
         (assert (and saw-early saw-late))
         (assert (some (lambda (stages) (= (length stages) 2))
                       (loop :for stages :being :the :hash-values :of clocks :collect stages)))
         (let ((crossing (routepoint-predecessor rp)))
           ;; Endpoint at 1.0 is itself a crossing, with fractional time retained.
           (loop :until (or (null crossing)
                            (< (abs (- (latlng-lng (routepoint-position crossing)) 1d0)) 1d-8))
                 :do (setf crossing (routepoint-predecessor crossing)))
           (assert crossing)
           (assert (> (timestamp-difference (routepoint-time crossing) start) 900)))
         (loop :for point = rp :then previous
               :for previous = (routepoint-predecessor point)
               :while previous
               :do (assert (timestamp<= (routepoint-time previous) (routepoint-time point)))))))
    ;; Multiple gates crossed in one step must be recorded in spatial order.
    (let* ((rp (create-routepoint nil (make-latlng :lat 0d0 :lng 0d0) start nil nil))
           (end (create-routepoint rp (make-latlng :lat 0d0 :lng 3d0)
                                  (adjust-timestamp start (:offset :sec 600))
                                  90d0 90d0 nil 10d0 0 0d0 10d0))
           (gate2 (make-gate :a (make-latlng :lat -1d0 :lng 2d0)
                            :b (make-latlng :lat 1d0 :lng 2d0)))
           (state (advance-gate-state (make-gate-state :point rp :origin rp) end
                                     (vector gate gate2 destination))))
      (assert (= (gate-state-stage state) 2))
      (assert (= (latlng-lng (routepoint-position (routepoint-predecessor end))) 2d0))
      (assert (= (latlng-lng (routepoint-position
                             (routepoint-predecessor (routepoint-predecessor end)))) 1d0)))
    ;; Gate progress must never share pruning buckets.
    (let* ((rp (create-routepoint nil destination start nil nil))
           (targets (vector gate destination))
           (buckets (make-hash-table :test #'equal)))
      (retain-gate-state (make-gate-state :point rp :origin rp :stage 0) buckets targets)
      (retain-gate-state (make-gate-state :point rp :origin rp :stage 1) buckets targets)
      (assert (= (hash-table-count buckets) 2)))
    ;; Refining a point arrival must not cut back before the last gate.
    (let* ((before (create-routepoint nil (make-latlng :lat 0d0 :lng 0d0) start nil nil))
           (crossing (create-routepoint before (make-latlng :lat 0d0 :lng 1d0)
                       (adjust-timestamp start (:offset :sec 300)) nil nil))
           (after (create-routepoint crossing (make-latlng :lat 0d0 :lng 1.5d0)
                    (adjust-timestamp start (:offset :sec 600)) nil nil))
           (visited nil))
      (call-with-gate-test-functions
       (list (cons 'approach-target (lambda (routing rp target)
                                     (declare (ignore routing target))
                                     (push rp visited) rp)))
       (lambda ()
         (best-approach routing after nil destination (routepoint-time crossing))
         (assert (member crossing visited))
         (assert (not (member before visited))))))
    (let* ((point (make-latlng :lat 1d0 :lng 1.5d0))
           (copy (copy-routing routing)))
      (setf (routing-waypoints copy) (list gate point gate))
      (let ((groups (group-gate-targets copy)))
        (assert (= (length groups) 2))
        (assert (equal (first groups) (list gate point)))
        (assert (equal (second groups) (list gate destination)))))
    (format t "~&Gate search regressions passed.~%")))

(defun test-gate-search-expansion ()
  "Exercise the real polar expansion and interpolation against a constant field."
  (let* ((start (parse-datetime "2026-10-30T00:00:00Z"))
         (grid (cl-weather::make-gribinfo :lat-start 1d0 :lat-end -1d0 :lat-points 3
                  :lon-start 0d0 :lon-end 360d0 :lon-points 361 :i-inc 1d0 :j-inc 1d0))
         (field (cl-weather::make-uv :cycle start :info grid
                  :u-array (make-array 1083 :element-type 'double-float :initial-element 0d0)
                  :v-array (make-array 1083 :element-type 'double-float :initial-element -10d0)))
         (params (cl-weather::make-params :fc0 field :fc1 field))
         (polars (make-cpolars :label "constant"
                   :maxspeed (make-array '(1801 101) :initial-element '(5d0 0))
                   :vmg (make-array 101 :initial-element
                          (list (make-vmg :twa 45d0) (make-vmg :twa 135d0)))))
         (routing (make-routing :start (make-latlng :lat 0d0 :lng 0.01d0)
                    :dest (make-latlng :lat 0d0 :lng 0.07d0) :starttime start :stepmax 1800
                    :polars polars :twa-angles (make-array 5 :element-type 'double-float
                                               :initial-contents '(45d0 90d0 135d0 45d0 135d0))))
         (gate (make-gate :a (make-latlng :lat -0.03d0 :lng 0.04d0)
                          :b (make-latlng :lat 0.03d0 :lng 0.04d0))))
    (call-with-gate-test-functions
     (list (cons 'get-params (lambda (&rest args) (declare (ignore args)) params))
           (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) nil)))
     (lambda ()
       (multiple-value-bind (info rp crossing) (get-route-through-gates routing (list gate))
         (assert (string= (routeinfo-status info) "reached"))
         (assert crossing)
         (assert (< (abs (- (latlng-lng (routepoint-position crossing)) 0.04d0)) 1d-8))
         (assert (timestamp< (routepoint-time crossing) (routepoint-time rp)))
         (assert (> (routestats-points (routeinfo-stats info)) 0))
         (assert (< (timestamp-difference (routepoint-time rp) start) 1800)))
       ;; Exercise the public waypoint coordinator, with the same real expansion.
       (setf (routing-waypoints routing) (list gate))
       (assert (string= (routeinfo-status (get-route-via-waypoints routing)) "reached"))
       (let ((short (copy-routing routing)))
         (setf (routing-stepmax short) 1)
         (assert (string= (routeinfo-status (get-route-through-gates short (list gate)))
                          "max_duration")))
       ;; An island on the direct course after the gate requires preserving
       ;; lateral expansion before reaching the blocked segment.
       (let* ((west (make-gate :a (make-latlng :lat -0.005d0 :lng 0.05d0)
                               :b (make-latlng :lat 0.005d0 :lng 0.05d0)))
              (east (make-gate :a (make-latlng :lat -0.005d0 :lng 0.055d0)
                               :b (make-latlng :lat 0.005d0 :lng 0.055d0)))
              (north (make-gate :a (gate-b west) :b (gate-b east)))
              (south (make-gate :a (gate-a west) :b (gate-a east))))
         (flet ((blocked (from to)
                  (or (and (<= 0.05d0 (latlng-lng to) 0.055d0)
                           (<= -0.005d0 (latlng-lat to) 0.005d0))
                      (some (lambda (edge) (gate-crossing edge from to))
                            (list west east north south)))))
           (call-with-gate-test-functions
            (list (cons 'approach-blocked-p #'blocked))
            (lambda ()
              (let* ((detour (copy-routing routing))
                     (info (progn (setf (routing-stepmax detour) 3600)
                                  (get-route-via-waypoints detour)))
                     (track (routeinfo-best info)))
                (assert (string= (routeinfo-status info) "reached"))
                (assert (some (lambda (point) (> (abs (latlng-lat (trackpoint-position point)))
                                                 0.005d0)) track))
                (loop :for (a b) :on track :while b
                      :do (assert (not (blocked (trackpoint-position a) (trackpoint-position b))))))))))
       (call-with-gate-test-functions
        (list (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) t)))
        (lambda ()
          (assert (string= (routeinfo-status (get-route-through-gates routing (list gate)))
                           "no_route"))))))
    (format t "~&Real expansion gate test passed.~%")))

(defun test-gate-frontier-pruning ()
  (let* ((start (parse-datetime "2026-10-30T00:00:00Z"))
         (origin (make-latlng :lat 38d0 :lng 0d0))
         (far (add-distance-exact origin 500000d0 90d0))
         (gate (make-gate :a (add-distance-exact far 10000d0 0d0)
                          :b (add-distance-exact far 10000d0 180d0)))
         (targets (vector gate far))
         (centers (vector origin (target-position gate)))
         (rp (create-routepoint nil origin start nil nil))
         (buckets (make-hash-table :test #'equal))
         (frontier nil)
         (wind (cl-weather::make-params :fc1 (cl-weather::make-uv :cycle start))))
    ;; A distant target must not compress a 180-degree origin fan into a
    ;; handful of destination-bearing buckets.
    (loop :for heading :from 0d0 :to 180d0 :by 10d0
          :for point = (create-routepoint rp (add-distance-exact origin 10000d0 heading)
                                         start nil nil)
          :for state = (make-gate-state :point point :origin rp)
          :do (retain-gate-state state buckets targets centers)
              (push state frontier))
    (assert (= (hash-table-count buckets) 19))
    ;; A second crossing origin must still use the common stage reference.
    (let* ((other (create-routepoint nil (add-distance-exact origin 500d0 0d0) start nil nil))
           (inner (create-routepoint rp (add-distance-exact origin 9000d0 90d0) start nil nil))
           (state (make-gate-state :point inner :origin other)))
      (retain-gate-state state buckets targets centers)
      (let* ((kept (gethash (gate-state-key state targets centers) buckets))
             (distance (course-distance origin (routepoint-position (gate-state-point kept)))))
        (assert (> distance 9900d0))))
    ;; Bucket selection is a direct replacement/discard decision. A blocked
    ;; winner is filtered once and leaves an empty bucket, with no fallback.
    (let* ((valid (make-gate-state :origin rp :source rp
                    :point (create-routepoint rp (add-distance-exact origin 10000d0 91d0) start nil nil)))
           (blocked (make-gate-state :origin rp :source rp
                      :point (create-routepoint rp (add-distance-exact origin 11000d0 91d0) start nil nil)))
           (inner (make-gate-state :origin rp :source rp
                    :point (create-routepoint rp (add-distance-exact origin 9000d0 91d0) start nil nil)))
           (winners (make-hash-table :test #'equal))
           (checked 0))
      (dolist (state (list valid blocked inner))
        (retain-gate-state state winners targets centers))
      (assert (eq blocked (gethash (gate-state-key blocked targets centers) winners)))
      (call-with-gate-test-functions
       (list (cons 'approach-blocked-p
                   (lambda (from to)
                     (assert (eq from origin))
                     (incf checked)
                     (> (course-distance from to) 10500d0))))
       (lambda ()
         (assert (null (select-gate-survivors winners)))
         (assert (= checked 1)))))
    ;; A better spatial neighbour replaces the winner, and a new exact tie
    ;; wins without a spacing test. Inferior candidates cannot displace it.
    (let ((winners (make-hash-table :test #'equal)))
      (flet ((candidate (distance)
               (make-gate-state :origin rp :source rp
                 :point (create-routepoint rp (add-distance-exact origin distance 91d0)
                                            start nil nil))))
        (let* ((winner (candidate 10600d0))
               (neighbour (candidate 10650d0))
               (tie (candidate 10650d0))
               (key (gate-state-key winner targets centers)))
          (retain-gate-state winner winners targets centers)
          (retain-gate-state (candidate 9000d0) winners targets centers)
          (assert (eq winner (gethash key winners)))
          (retain-gate-state neighbour winners targets centers)
          (assert (eq neighbour (gethash key winners)))
          (retain-gate-state tie winners targets centers)
          (assert (eq tie (gethash key winners))))))
    ;; Inner survivors at the same bearing cannot create radial spokes. A gap
    ;; in the angular envelope produces separate paths, not a chord across it.
    (let ((inner (create-routepoint rp (add-distance-exact origin 2000d0 90d0) start nil nil)))
      (push (make-gate-state :point inner :origin rp) frontier))
    (let ((paths (gate-frontier-isochrones frontier centers start wind 3000d0)))
      (assert (= (length paths) 19))
      (dolist (iso paths)
        (assert (= (isochrone-stage iso) 0))
        (assert (= (length (isochrone-path iso)) 1))
        (assert (> (course-distance origin (first (isochrone-path iso))) 9900d0)))))
  (format t "~&Gate frontier width and rendering regressions passed.~%"))

(defun test-gate-retirement-and-progress ()
  (let* ((origin (make-latlng :lat 38d0 :lng 0d0))
         (time (now))
         (rp (create-routepoint nil origin time nil nil))
         (centers (vector origin origin))
         (targets (vector origin origin))
         (*gate-progress* (make-hash-table :test #'equal)))
    (flet ((state (distance stage)
             (make-gate-state :stage stage :origin rp
               :point (create-routepoint rp (add-distance-exact origin distance 91d0) time nil nil))))
      (record-gate-progress (list (state 10000d0 0)) centers)
      ;; Empty buckets in a later step must not forget earlier outward progress.
      (let ((buckets (make-hash-table :test #'equal)))
        (retain-gate-state (state 9000d0 0) buckets targets centers)
        (retain-gate-state (state 10000d0 0) buckets targets centers)
        (assert (zerop (hash-table-count buckets)))
        (retain-gate-state (state 11000d0 0) buckets targets centers)
        (assert (= (hash-table-count buckets) 1))
        ;; Crossing histories at another stage have independent progress marks.
        (retain-gate-state (state 9000d0 1) buckets targets centers)
        (assert (= (hash-table-count buckets) 2)))))
  (let* ((*gate-retirement-coverage* 1)
         (gate (make-gate :a (make-latlng :lat 0d0 :lng 1d0)
                          :b (make-latlng :lat 1d0 :lng 1d0)))
         (gates (vector gate))
         (coverage (vector (make-array 4 :element-type 'bit :initial-element 0))))
    (flet ((hit (fraction)
             (make-gate-state :stage 1
               :crossings (list (cons 0 (make-latlng :lat fraction :lng 1d0))))))
      (record-gate-coverage (list (hit 0.1d0) (hit 0.4d0) (hit 0.6d0)) gates coverage)
      (assert (not (gate-stage-retired-p 0 coverage)))
      ;; Repeated arrivals in one interval cannot stand in for a missing interval.
      (record-gate-coverage (list (hit 0.15d0)) gates coverage)
      (assert (not (gate-stage-retired-p 0 coverage)))
      (record-gate-coverage (list (hit 1d0)) gates coverage)
      (assert (gate-stage-retired-p 0 coverage))
      (assert (not (gate-stage-retired-p 1 coverage)))))
  ;; Half the distinct intervals suffice by default; strict mode remains available.
  (let* ((*gate-retirement-coverage* 1/2)
         (bins (make-array 12 :element-type 'bit :initial-element 0))
         (coverage (vector bins)))
    (fill bins 1 :end 5)
    (assert (not (gate-stage-retired-p 0 coverage)))
    (setf (aref bins 11) 1)
    (assert (gate-stage-retired-p 0 coverage))
    (assert (not (gate-stage-retired-p 1 coverage)))
    (let ((*gate-retirement-coverage* 1))
      (assert (not (gate-stage-retired-p 0 coverage)))
      (fill bins 1)
      (assert (gate-stage-retired-p 0 coverage))))
  ;; Round half coverage up for odd interval counts.
  (let ((*gate-retirement-coverage* 1/2))
    (assert (not (gate-stage-retired-p 0 (vector #*11000))))
    (assert (gate-stage-retired-p 0 (vector #*11100))))
  (format t "~&Gate retirement and persistent progress regressions passed.~%"))

;;; Check linear result selection across stages, distance ordering and exact ties.
(defun test-gate-result-selection ()
  (let* ((time (now))
         (destination (make-latlng :lat 0d0 :lng 3d0))
         (targets (vector destination destination)))
    (flet ((state (stage longitude)
             (make-gate-state :stage stage
               :point (create-routepoint nil (make-latlng :lat 0d0 :lng longitude)
                                          time nil nil))))
      (let* ((early (state 0 2.9d0))
             (advanced (state 1 1d0))
             (nearer (state 1 2d0))
             (tie (state 1 2d0)))
        (assert (eq advanced (best-gate-state (list early advanced) targets)))
        (assert (eq nearer (best-gate-state (list early advanced nearer tie) targets)))
        (assert (eq early (best-gate-state (list early advanced nearer) targets t)))
        (assert (eq nearer (best-gate-state (list nearer tie) targets t)))
        (assert (null (best-gate-state nil targets))))))
  (format t "~&Gate result selection regressions passed.~%"))
