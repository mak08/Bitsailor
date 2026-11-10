;;; Load after gate-search-tests.cl and fine-tune-tests.cl.
(in-package :bitsailor)

;;; Rolling candidates must overlap, cover multiple horizons and respect hard
;;; mark boundaries even when the lateral corridor is much wider.
(defun test-fine-rolling ()
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                    (loop repeat 60 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (track (routeinfo-best info)) (start (trackpoint-time (first track)))
            (specs (fine-rolling-specs routing track)))
       (assert (some (lambda (s) (= 14400d0 (timestamp-difference (second s) (first s)))) specs))
       (assert (some (lambda (s) (= 1800d0 (timestamp-difference (second s) (first s)))) specs))
       (assert (some (lambda (s) (timestamp> (first s) start)) specs))
       (let ((mark (nth 60 track)))
         (setf (routing-waypoints routing) (list (trackpoint-position mark)))
         (dolist (s (fine-rolling-specs routing track))
           (assert (not (and (timestamp< (first s) (trackpoint-time mark))
                             (timestamp> (second s) (trackpoint-time mark))))))))))
  ;; A validated improvement must survive failures of later overlapping windows.
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                    (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)) (calls 0)
            (optimizer (symbol-function 'fine-optimize-window)))
       (let ((*fine-tune-enabled* nil)) (fine-tune-route routing info))
       (call-with-gate-test-functions
        (list (cons 'fine-optimize-window
                    (lambda (&rest args)
                      (incf calls)
                      (if (= calls 1) (apply optimizer args)
                          (error "test later rolling window failure")))))
        (lambda () (fine-tune-rolling routing info)))
       (format t "~&Rolling incumbent: ~a -> ~a changes, ~a accepted windows~%"
               (fine-changes raw) (fine-changes (routeinfo-best info))
               (fine-tuning-rolling-improvements (routeinfo-fine-tuning info)))
       (assert (> calls 1))
       (assert (plusp (fine-tuning-rolling-improvements (routeinfo-fine-tuning info))))
       (assert (< (fine-changes (routeinfo-best info)) (fine-changes raw)))
       (assert (fine-minute-plan-p (routeinfo-best info)))
       (assert (timestamp<= (trackpoint-time (car (last (routeinfo-best info))))
                            (trackpoint-time (car (last raw)))))
       (assert (fine-check-targets routing (routeinfo-best info))))))
  ;; Marked routes can improve, but the physical trajectory must still hit the
  ;; exact mark; the mark is not assigned the five-nautical-mile corridor radius.
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                    (loop repeat 16 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)) (mark (trackpoint-position (nth 16 raw))))
       (setf (routing-waypoints routing) (list mark))
       (let ((*fine-tune-enabled* nil)) (fine-tune-route routing info))
       (fine-tune-rolling routing info)
       (format t "~&Rolling marked route: ~a -> ~a changes~%" (fine-changes raw) (fine-changes (routeinfo-best info)))
       (assert (< (fine-changes (routeinfo-best info)) (fine-changes raw)))
       (assert (fine-check-targets routing (routeinfo-best info)))
       (assert (<= (reduce #'min (routeinfo-best info)
                            :key (lambda (p) (fine-distance mark (trackpoint-position p))))
                   *fine-tune-mark-tolerance*))
       (assert (timestamp<= (trackpoint-time (car (last (routeinfo-best info)))) (trackpoint-time (car (last raw))))))))
  ;; Unique fractional trial timestamps cannot grow the scratch cache without bound.
  (call-with-fine-test
   (lambda (routing)
     (let ((*fine-tune-weather-cache-limit* 8))
       (dotimes (i 100)
         (fine-weather routing (fine-time+ (routing-starttime routing) (* i 0.1d0)))
         (assert (<= (hash-table-count *fine-tune-weather*) 8))))))
  ;; A new physical suffix starts at the proposed endpoint, never the old anchor.
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing (loop repeat 8 collect (cons 105d0 120d0))))
            (raw (routeinfo-best info))
            (patch (mapcar #'copy-trackpoint (subseq raw 0 4)))
            (end (car (last patch)))
            (moved (add-distance-exact (trackpoint-position end) 5d0 90d0)))
       (setf (trackpoint-position end) moved)
       (let* ((whole (fine-replay-patch routing raw 0 3 patch))
              (junction (find (trackpoint-time end) whole :key #'trackpoint-time :test #'timestamp=)))
         (assert (< (fine-distance moved (trackpoint-position junction)) 0.00001d0))
         (assert (> (fine-distance (trackpoint-position (nth 3 raw)) (trackpoint-position junction)) 4d0))))))
  ;; The broad corridor is not a larger target radius.
  (let* ((origin (make-latlng :lat 0d0 :lng 0d0))
         (mark (add-distance-exact origin 1000d0 90d0))
         (near (add-distance-exact mark 0.5d0 0d0))
         (time (parse-datetime "2026-11-07T12:00:00Z"))
         (a (make-trackpoint :position origin :time time :twa 90d0))
         (b (make-trackpoint :position mark :time (fine-time+ time 600d0) :twa 90d0))
         (c (make-trackpoint :position near :time (trackpoint-time b) :twa 90d0))
         (routing (make-routing :start origin :dest mark :waypoints (list mark)))
         (*fine-tune-position-limit* 9260d0))
    (assert (fine-check-targets routing (list a b)))
    (assert (not (fine-check-targets routing (list a c))))
    (assert (fine-position-admissible-p mark near))
    (let ((middle (add-distance-exact origin 500d0 90d0)))
      (setf (routing-waypoints routing) (list middle mark))
      (assert (fine-check-targets routing (list a b)))
      (setf (routing-waypoints routing) (list mark middle))
      (assert (not (fine-check-targets routing (list a b))))))
  ;; A partial endpoint closer to the destination must not move away from an
  ;; outstanding intermediate mark that still has to be visited.
  (let* ((origin (make-latlng :lat 0d0 :lng 0d0))
         (old (add-distance-exact origin 10000d0 0d0))
         (ahead (add-distance-exact origin 11000d0 0d0))
         (next (add-distance-exact origin 5000d0 90d0))
         (time (parse-datetime "2026-11-07T12:00:00Z"))
         (a (make-trackpoint :position origin :time time :twa 90d0))
         (b (make-trackpoint :position old :time (fine-time+ time 600d0) :twa 90d0))
         (c (make-trackpoint :position ahead :time (trackpoint-time b) :twa 90d0))
         (routing (make-routing :start origin :dest (add-distance-exact origin 20000d0 0d0) :waypoints (list next))))
    (assert (< (course-distance (routing-dest routing) ahead) (course-distance (routing-dest routing) old)))
    (assert (not (fine-mark-progress-safe-p routing (list a b) (list a c)))))
  (format t "~&Rolling horizons, retained improvements, physical suffix continuity and exact marks passed.~%"))
