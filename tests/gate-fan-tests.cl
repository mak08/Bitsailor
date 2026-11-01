;;; Load after Bitsailor and gate-search-tests.cl, then call TEST-GATE-CROSSING-FAN.
(in-package :bitsailor)

;;; Verify the retained frontier against crossing-centred fans using real expansion.
;;; The raised gate makes midpoint-centred and crossing-centred directions differ.
(defun test-gate-crossing-fan ()
  (let* ((start (parse-datetime "2026-10-31T00:00:00Z"))
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
         (angles (coerce (loop :for angle :from 45 :to 135 :by 5 :collect (float angle 1d0))
                         '(vector double-float)))
         (routing (make-routing :start (make-latlng :lat 0d0 :lng 0d0)
                    :dest (make-latlng :lat 0d0 :lng 0.14d0) :starttime start :stepmax 3600
                    :polars polars :twa-angles (make-twa-angles-buffer (make-cpolars :twa angles))))
         (gate (make-gate :a (make-latlng :lat 0.02d0 :lng 0.04d0)
                          :b (make-latlng :lat 0.12d0 :lng 0.04d0)))
         (original (symbol-function 'record-gate-coverage))
         (checked 0))
    (call-with-gate-test-functions
     (list (cons 'get-params (lambda (&rest args) (declare (ignore args)) params))
           (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) nil))
           (cons 'record-gate-coverage
                 (lambda (states targets coverage)
                   (funcall original states targets coverage)
                   (dolist (state states)
                     ;; Every retained stage-1 state, including a new crossing,
                     ;; must lie in the fan from its own crossing point.
                     (when (= (gate-state-stage state) 1)
                       (let* ((origin (routepoint-position (gate-state-origin state)))
                              (pos (routepoint-position (gate-state-point state)))
                              (delta (abs (normalize-angle
                                           (- (gate-course-angle origin pos)
                                              (gate-course-angle origin (routing-dest routing)))))))
                         (assert (<= delta (+ (routing-fan routing) 1d-6)))
                         (incf checked)))))))
     (lambda ()
       (assert (string= "reached" (routeinfo-status (get-route-through-gates routing (list gate)))))
       (assert (> checked 100))))
    ;; The point annotated in the original figure is outside the crossing fan,
    ;; although it was inside the former midpoint-centred fan.
    (let* ((crossing (make-latlng :lat 0.0202d0 :lng 0.04d0))
           (midpoint (target-position gate))
           (point (make-latlng :lat 0.0099d0 :lng 0.0094d0)))
      (assert (> (abs (normalize-angle (- (gate-course-angle crossing point)
                                         (gate-course-angle crossing (routing-dest routing)))))
                 (routing-fan routing)))
      (assert (< (abs (normalize-angle (- (gate-course-angle midpoint point)
                                         (gate-course-angle midpoint (routing-dest routing)))))
                 (routing-fan routing)))))
  (format t "~&Crossing-origin search-fan regression passed.~%"))
