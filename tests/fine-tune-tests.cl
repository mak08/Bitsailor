;;; Load after Bitsailor and gate-search-tests.cl, then call TEST-FINE-TUNING.
(in-package :bitsailor)

;;; Build a synthetic forecast with constant speed and optionally rotating direction.
(defun fine-test-weather (start time grid rotating)
  (let* ((direction (rad (if rotating (* 0.01d0 (timestamp-difference time start)) 0d0)))
         (field (cl-weather::make-uv :cycle start :info grid
                  :u-array (make-array 1083 :element-type 'double-float :initial-element (* -10d0 (sin direction)))
                  :v-array (make-array 1083 :element-type 'double-float :initial-element (* -10d0 (cos direction))))))
    (cl-weather::make-params :fc0 field :fc1 field)))

;;; Run one physical fixture without map/forecast downloads, using the real interpolation/polars.
(defun call-with-fine-test (thunk &optional rotating)
  (let* ((start (parse-datetime "2026-11-03T12:00:00Z"))
         (origin (make-latlng :lat 0d0 :lng 1d0))
         (grid (cl-weather::make-gribinfo :lat-start 1d0 :lat-end -1d0 :lat-points 3
                  :lon-start 0d0 :lon-end 360d0 :lon-points 361 :i-inc 1d0 :j-inc 1d0))
         (polars (make-cpolars :label "constant"
                  :maxspeed (make-array '(1801 101) :initial-element '(5d0 0))
                  :vmg (make-array 101 :initial-element
                         (list (make-vmg :twa 45d0) (make-vmg :twa 135d0)))))
         (routing (make-routing :start origin :dest (make-latlng :lat 0d0 :lng 2d0)
                    :starttime start :polars polars :stepmax 3600))
         (*fine-tune-max-seconds* 60d0)
         (*fine-tune-weather* (make-hash-table :test #'equal))
         (*fine-tune-evaluations* 0)
         (*fine-tune-started* (get-internal-real-time)))
    (call-with-gate-test-functions
     (list
      (cons 'get-params
            (lambda (model time &rest args)
              (declare (ignore model args))
              (fine-test-weather start time grid rotating)))
      (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) nil)))
     (lambda () (funcall thunk routing)))))

;;; Produce a reference using the original coarse integration and outgoing TWA semantics.
(defun fine-test-route (routing controls)
  (let* ((track (fine-replay routing (routing-start routing) (routing-starttime routing)
                            controls :step nil :counted nil))
         (end (car (last track))))
    (assert track)
    (setf (routing-dest routing) (trackpoint-position end))
    (make-routeinfo :status "reached" :best track :forecast-model 'cl-weather:noaa-gfs-wind
                   :initial-cycle (routing-starttime routing) :stats (get-statistics track 0d0 1 1))))

;;; Check outcomes rather than numerical solver internals or selected angle values.
(defun fine-test-check (raw info)
  (let* ((track (routeinfo-best info)) (report (routeinfo-fine-tuning info))
         (original-end (car (last raw))) (end (car (last track))))
    (format t "~&Fine tuning: ~a / ~a (~a evaluations), changes ~a -> ~a~%"
            (fine-tuning-status report) (fine-tuning-reason report) (fine-tuning-evaluations report)
            (fine-changes raw) (fine-changes track))
    (assert (string= (fine-tuning-status report) "optimized"))
    (assert (< (fine-changes track) (fine-changes raw)))
    (assert (timestamp<= (trackpoint-time end) (trackpoint-time original-end)))
    (assert (<= (fine-distance (trackpoint-position original-end) (trackpoint-position end))
                *fine-tune-endpoint-tolerance*))
    (assert (fine-check-manoeuvres raw track))
    (assert (routeinfo-twa-segments info))
    (assert (routeinfo-original-best info))))

;;; Check the motivating jitter, tack/gybe constraints, genuine rotating-wind TWA sailing,
;;; and safe fallback on land rejection or an exhausted optimizer budget.
(defun test-fine-tuning ()
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                   (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)))
       (fine-tune-route routing info)
       (fine-test-check raw info)
       (assert (= (length (routeinfo-twa-segments info)) 1)))))
  (dolist (angles '((45d0 50d0 :tack) (125d0 130d0 :gybe)))
    (call-with-fine-test
     (lambda (routing)
       (let* ((a (first angles)) (b (second angles))
              (controls (append (loop repeat 3 append (list (cons a 120d0) (cons b 120d0)))
                                (loop repeat 3 append (list (cons (- a) 120d0) (cons (- b) 120d0)))))
              (info (fine-test-route routing controls)) (raw (routeinfo-best info)))
         (fine-tune-route routing info)
         (fine-test-check raw info)
         (assert (= (length (fine-manoeuvres (routeinfo-best info))) 1))
         (assert (eq (third angles) (fine-manoeuvre-kind (first (fine-manoeuvres (routeinfo-best info))))))))))
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                   (loop repeat 4 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)) (*fine-tune-max-evaluations* 0))
       (fine-tune-route routing info)
       (assert (eq raw (routeinfo-best info))))))
  ;; A smoother arc which misses a short required gate must be rolled back as a
  ;; whole route, even though it closes at the final endpoint and avoids land.
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                   (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)) (touch (trackpoint-position (second raw))))
       (setf (routing-waypoints routing)
             (list (make-gate :a (add-distance-exact touch 5d0 0d0)
                              :b (add-distance-exact touch 5d0 180d0))))
       (assert (fine-check-targets routing raw))
       (fine-tune-route routing info)
       (assert (eq raw (routeinfo-best info))))))
  ;; Missing forecast data in post-processing cannot break a successful route.
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                   (loop repeat 4 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)))
       (call-with-gate-test-functions
        (list (cons 'get-params (lambda (&rest args) (declare (ignore args)) (error "test missing forecast"))))
        (lambda () (fine-tune-route routing info)))
       (assert (eq raw (routeinfo-best info))))))
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                   (loop repeat 4 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)))
       (call-with-gate-test-functions
        (list (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) t)))
        (lambda () (fine-tune-route routing info)))
       (assert (eq raw (routeinfo-best info))))))
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                   (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
            (raw (routeinfo-best info)))
       (fine-tune-route routing info)
       (fine-test-check raw info)
       (let ((p (first (routeinfo-best info))) (q (second (routeinfo-best info))))
         (assert (> (abs (- (trackpoint-heading p) (trackpoint-heading q))) 0.1d0))
         (assert (< (abs (- (trackpoint-twa p) (trackpoint-twa q))) 1d-6)))))
   t)
  ;; Rotating wind with an actual manoeuvre, not just a same-tack arc.
  (call-with-fine-test
   (lambda (routing)
     (let* ((controls (append (loop repeat 3 append (list (cons 45d0 120d0) (cons 50d0 120d0)))
                              (loop repeat 3 append (list (cons -45d0 120d0) (cons -50d0 120d0)))))
            (info (fine-test-route routing controls)) (raw (routeinfo-best info)))
       (fine-tune-route routing info)
       (fine-test-check raw info)
       (format t "~&Rotating-wind tack movement: ~,2fm / ~,2fs~%"
               (fine-tuning-max-manoeuvre-distance (routeinfo-fine-tuning info))
               (fine-tuning-max-manoeuvre-seconds (routeinfo-fine-tuning info)))))
   t)
  ;; Mandatory point targets and gate order must survive a geometrically smoother shortcut.
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing (list (cons 90d0 200d0))))
            (track (routeinfo-best info)) (end (trackpoint-position (car (last track))))
            (gate (make-gate :a (add-distance-exact end 100d0 0d0)
                             :b (add-distance-exact end 100d0 180d0))))
       (setf (routing-waypoints routing) (list gate))
       (assert (fine-check-targets routing track))
       (setf (routing-waypoints routing) (list (add-distance-exact end 100d0 0d0)))
       (assert (not (fine-check-targets routing track)))
       (setf (routing-waypoints routing) nil
             (routing-forecast-switch-time routing) (fine-time+ (routing-starttime routing) 30d0)
             (routing-secondary-forecast-model routing) 'cl-weather::ecmwf-ifs-wind
             *fine-tune-weather* (make-hash-table :test #'equal))
       (let ((split (fine-replay routing (routing-start routing) (routing-starttime routing)
                                 (list (cons 90d0 120d0)) :counted nil)))
         (assert (= (timestamp-difference (trackpoint-time (second split)) (routing-starttime routing)) 30d0))
         (assert (eq (trackpoint-wind-model (second split)) 'cl-weather::ecmwf-ifs-wind))))))
  (format t "~&TWA smoothing, endpoint closure, tack/gybe preservation, rotating wind and rollback tests passed.~%"))

;;; Real scalar/vector interpolation must preserve the router's wave/current physics.
(defun test-fine-tune-physics ()
  (call-with-fine-test
   (lambda (routing)
     (let* ((time (routing-starttime routing))
            (grid (cl-weather::make-gribinfo :lat-start 1d0 :lat-end -1d0 :lat-points 3
                     :lon-start 0d0 :lon-end 360d0 :lon-points 361 :i-inc 1d0 :j-inc 1d0))
            (wind (fine-test-weather time time grid nil))
            (flow (cl-weather::make-uv :cycle time :info grid
                    :u-array (make-array 1083 :element-type 'double-float :initial-element 1d0)
                    :v-array (make-array 1083 :element-type 'double-float :initial-element 0d0)))
            (height (cl-weather::make-gribmessage :info grid
                      :data (make-array 1083 :element-type 'double-float :initial-element 4d0)))
            (direction (cl-weather::make-gribmessage :info grid
                         :data (make-array 1083 :element-type 'double-float :initial-element 270d0))))
       (setf (routing-currents routing) :test-current (routing-use-waves routing) t
             *fine-tune-weather* (make-hash-table :test #'equal))
       (call-with-gate-test-functions
        (list
         (cons 'get-params
               (lambda (model timestamp &rest args)
                 (declare (ignore timestamp args))
                 (case model
                   (:test-current (cl-weather::make-params :fc0 flow :fc1 flow))
                   (cl-weather:gfswave-combined (cl-weather::make-params :fc0 height :fc1 height))
                   (t wind))))
         (cons 'get-direction-params
               (lambda (&rest args)
                 (declare (ignore args))
                 (cl-weather::make-params :fc0 direction :fc1 direction))))
        (lambda ()
          (multiple-value-bind (sample endpoint)
              (fine-sail-step routing (routing-start routing) time 100d0 90d0)
            (assert (< (abs (- (trackpoint-speed sample) 5.5d0)) 1d-8))
            (assert (< (abs (- (course-distance (routing-start routing) endpoint) 650d0)) 1d0))
            (assert (= (trackpoint-twa sample) 90d0))))))))
  (format t "~&Fine-tuning wave/current physics regression passed.~%"))
