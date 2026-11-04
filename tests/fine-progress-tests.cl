;;; Load after Bitsailor, gate-search-tests.cl and fine-tune-tests.cl.
(in-package :bitsailor)

;;; Duration-limited routes must gain radial distance at the identical clock horizon,
;;; preserving integer commands and the lateral corridor rather than closing the old endpoint.
(defun test-fine-progress ()
  (call-with-fine-test
   (lambda (routing)
     (let* ((*fine-tune-outward-enabled* t)
            (info (fine-test-route routing
                    (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info))
            (origin (routing-start routing))
            (old-end (car (last raw)))
            (*fine-tune-progress-origin* origin))
       (setf (routeinfo-status info) "max_duration"
             (routing-dest routing) (add-distance-exact origin 100000d0 107.5d0))
       ;; A pending gate is not a gate already passed: do not reject a valid
       ;; partial route merely because it has not reached that gate yet.
       (let ((centre (add-distance-exact origin 50000d0 107.5d0)))
         (setf (routing-waypoints routing)
               (list (make-gate :a (add-distance-exact centre 500d0 0d0)
                                :b (add-distance-exact centre 500d0 180d0)))))
       (fine-tune-route routing info)
       (let* ((track (routeinfo-best info)) (end (car (last track)))
              (report (routeinfo-fine-tuning info))
              (gain (- (course-distance origin (trackpoint-position end))
                       (course-distance origin (trackpoint-position old-end)))))
         (format t "~&Partial-route progress: ~a / ~a, gain=~,3fm, changes ~a -> ~a~%"
                 (fine-tuning-status report) (fine-tuning-reason report) gain (fine-changes raw) (fine-changes track))
         (assert (string= (routeinfo-status info) "max_duration"))
         (assert (string= (fine-tuning-status report) "optimized"))
         (assert (string= (fine-tuning-objective report) "origin-distance"))
         (assert (> gain 5d0))
         (assert (timestamp= (trackpoint-time end) (trackpoint-time old-end)))
         (assert (fine-integer-plan-p track))
         (assert (fine-check-manoeuvres raw track))
         (dolist (p track)
           (assert (fine-position-admissible-p (fine-reference-position raw (trackpoint-time p))
                                               (trackpoint-position p))))))))
  ;; A speculative outward pass must preserve the established completed-route
  ;; incumbent, including its earlier arrival and closed destination.
  (call-with-fine-test
   (lambda (routing)
     (let* ((raw (fine-test-route routing
                   (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (baseline (copy-routeinfo raw)) (outward (copy-routeinfo raw)))
       (fine-tune-route routing baseline)
       (let ((*fine-tune-outward-enabled* t)) (fine-tune-route routing outward))
       (assert (timestamp<= (trackpoint-time (car (last (routeinfo-best outward))))
                            (trackpoint-time (car (last (routeinfo-best baseline))))))
       (assert (<= (fine-distance (routing-dest routing)
                                  (trackpoint-position (car (last (routeinfo-best outward)))))
                   *fine-tune-endpoint-tolerance*))
       (assert (<= (fine-tuning-evaluations (routeinfo-fine-tuning outward))
                   *fine-tune-max-evaluations*)))))
  ;; A failed speculative pass must retain the valid fixed-horizon incumbent.
  (call-with-fine-test
   (lambda (routing)
     (let* ((*fine-tune-outward-enabled* t)
            (info (fine-test-route routing
                    (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (optimizer (symbol-function 'fine-optimize-window)))
       (setf (routeinfo-status info) "max_duration")
       (call-with-gate-test-functions
        (list (cons 'fine-optimize-window
                    (lambda (&rest args)
                      (if *fine-tune-suffix-only* (apply optimizer args)
                          (error "test speculative pass failure")))))
        (lambda () (fine-tune-route routing info)))
       (assert (string= (fine-tuning-status (routeinfo-fine-tuning info)) "optimized"))
       (assert (> (fine-tuning-distance-gain (routeinfo-fine-tuning info)) 5d0)))))
  ;; The great-circle corridor must remain a ray, not its opposite half; verify
  ;; dateline wrapping and the lateral bound independently of the steering search.
  (let* ((*fine-tune-outward-enabled* t)
         (origin (make-latlng :lat 0d0 :lng 179.9d0))
         (*fine-tune-progress-origin* origin)
         (reference (add-distance-exact origin 10000d0 90d0))
         (ahead (add-distance-exact origin 30000d0 90d0))
         (opposite (add-distance-exact origin 30000d0 270d0)))
    (assert (fine-position-admissible-p reference ahead))
    (assert (not (fine-position-admissible-p reference opposite)))
    (assert (fine-position-admissible-p reference (add-distance-exact ahead 499d0 0d0)))
    (assert (not (fine-position-admissible-p reference (add-distance-exact ahead 501d0 0d0))))
    (let* ((time (parse-datetime "2026-11-04T12:00:00Z"))
           (a (make-trackpoint :time time :position origin :twa 90d0))
           (b (make-trackpoint :time (fine-time+ time 600d0) :position reference :twa 90d0))
           (c (make-trackpoint :time (fine-time+ time 600d0) :position ahead :twa 90d0))
           (routing (make-routing :start origin :dest ahead))
           (raw (list a b)) (candidate (list a c)))
      (assert (fine-free-anchor-p routing raw t t))
      (assert (not (fine-free-anchor-p routing raw t nil)))
      (assert (fine-result-valid-p routing raw candidate t))
      (setf (trackpoint-time c) (fine-time+ (trackpoint-time b) 1d0))
      (assert (not (fine-result-valid-p routing raw candidate t)))
      (setf (trackpoint-time c) (fine-time+ (trackpoint-time b) -1d0))
      (assert (not (fine-result-valid-p routing raw candidate t)))
      (setf (trackpoint-time c) (trackpoint-time b)
            (routing-waypoints routing) (list reference))
      (assert (not (fine-free-anchor-p routing raw t t)))
      (assert (not (fine-result-valid-p routing raw candidate t)))
      ;; A partial route must retain gates it had already touched, even though
      ;; later targets can remain unvisited.
      (let ((centre (add-distance-exact origin 5000d0 90d0)))
        (setf (routing-waypoints routing)
              (list (make-gate :a (add-distance-exact centre 50d0 0d0)
                               :b (add-distance-exact centre 50d0 180d0)))
              (trackpoint-position c) (add-distance-exact ahead 499d0 0d0))
        (assert (= 1 (fine-target-progress routing raw)))
        (assert (not (fine-result-valid-p routing raw candidate t)))))
    ;; Outward manoeuvre movement can exceed the old 500 m disk, but not the
    ;; lateral/time bounds. This is still the same tack, not permission to add one.
    (let* ((time (parse-datetime "2026-11-04T12:00:00Z"))
           (a (make-trackpoint :time time :position origin :twa 45d0))
           (b (make-trackpoint :time (fine-time+ time 600d0) :position reference :twa -45d0))
           (c (make-trackpoint :time (fine-time+ time 1200d0) :position ahead :twa -45d0))
           (moved (copy-trackpoint b)))
      (setf (trackpoint-position moved) ahead (trackpoint-time moved) (fine-time+ (trackpoint-time b) 60d0))
      (assert (fine-check-manoeuvres (list a b c) (list a moved c)))
      (setf (trackpoint-position moved) (add-distance-exact ahead 501d0 0d0))
      (assert (not (fine-check-manoeuvres (list a b c) (list a moved c))))))
  (format t "~&Fixed-horizon progress, lateral rays, waypoint constraints and manoeuvre regressions passed.~%"))
