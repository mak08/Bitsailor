;;; Load after Bitsailor, gate-search-tests.cl and fine-tune-tests.cl.
(in-package :bitsailor)

;;; A same-side 5-degree TWA alternation can describe one compass heading when
;;; wind oscillates. Test actual replay, metadata and no-later-ETA acceptance.
(defun test-fine-heading ()
  (call-with-fine-test
   (lambda (routing)
     (let* ((start (routing-starttime routing))
            (grid (cl-weather::make-gribinfo :lat-start 1d0 :lat-end -1d0 :lat-points 3
                     :lon-start 0d0 :lon-end 360d0 :lon-points 361 :i-inc 1d0 :j-inc 1d0))
            (flow (cl-weather::make-uv :cycle start :info grid
                    :u-array (make-array 1083 :element-type 'double-float :initial-element 0d0)
                    :v-array (make-array 1083 :element-type 'double-float :initial-element 1d0)))
            (*fine-tune-heading-enabled* t))
       (setf (routing-currents routing) :heading-test-current)
       (call-with-gate-test-functions
        (list (cons 'get-params
                    (lambda (model time &rest options)
                      (declare (ignore options))
                      (if (eq model :heading-test-current)
                          (cl-weather::make-params :fc0 flow :fc1 flow)
                          (let* ((seconds (timestamp-difference time start))
                             (direction (rad (if (evenp (floor seconds 120d0)) 0d0 5d0)))
                             (uv (cl-weather::make-uv :cycle start :info grid
                                   :u-array (make-array 1083 :element-type 'double-float :initial-element (* -10d0 (sin direction)))
                                   :v-array (make-array 1083 :element-type 'double-float :initial-element (* -10d0 (cos direction))))))
                            (cl-weather::make-params :fc0 uv :fc1 uv))))))
        (lambda ()
          (let* ((info (fine-test-route routing
                        (loop repeat 8 append (list (cons 110d0 120d0) (cons 105d0 120d0)))))
                 (raw (routeinfo-best info)))
            (assert (fine-heading-stretch-p raw))
            (fine-tune-route routing info)
            (let* ((track (routeinfo-best info)) (plan (routeinfo-steering-segments info)))
              (format t "~&Heading fit: ~a / ~a; ~a -> ~a commanded changes~%"
                      (fine-tuning-status (routeinfo-fine-tuning info))
                      (fine-tuning-reason (routeinfo-fine-tuning info)) (fine-changes raw) (fine-changes track))
              (assert (string= (fine-tuning-status (routeinfo-fine-tuning info)) "optimized"))
              (assert (= (length plan) 1))
              (assert (string= (steering-segment-mode (first plan)) "heading"))
              (assert (= (steering-segment-value (first plan)) 110))
              (assert (null (routeinfo-twa-segments info)))
              (assert (fine-integer-plan-p track))
              (assert (zerop (fine-changes track)))
              (assert (timestamp<= (trackpoint-time (car (last track))) (trackpoint-time (car (last raw)))))
              (assert (<= (fine-distance (trackpoint-position (car (last track)))
                                         (trackpoint-position (car (last raw)))) *fine-tune-endpoint-tolerance*))
              (assert (> (abs (- (trackpoint-twa (first track))
                                   (trackpoint-twa (find-if (lambda (p) (>= (timestamp-difference (trackpoint-time p) start) 120d0)) track)))) 4d0))
              (assert (= (trackpoint-control-value (first track)) (trackpoint-control-value (second track))))
              (assert (> (abs (- (trackpoint-heading (first track)) 110d0)) 1d0))
              (assert (fine-check-manoeuvres raw track)))
            ;; Re-run the same physically eligible fit with blocked segments:
            ;; heading mode must not bypass the usual land/rollback checks.
            (let ((blocked (make-routeinfo :status "reached" :best raw :stats (routeinfo-stats info))))
              (call-with-gate-test-functions
               (list (cons 'approach-blocked-p (lambda (&rest args) (declare (ignore args)) t)))
               (lambda () (fine-tune-route routing blocked)))
              (assert (eq raw (routeinfo-best blocked))))))))))
  ;; Monotone TWA transitions and real manoeuvres are deliberately ineligible.
  (call-with-fine-test
   (lambda (routing)
     (dolist (angles '((100d0 105d0 110d0) (110d0 -110d0 110d0)))
       (let ((info (fine-test-route routing (mapcar (lambda (a) (cons a 120d0)) angles))))
         (assert (not (fine-heading-stretch-p (routeinfo-best info))))))))
  ;; A fixed compass heading is not a fixed ground course in current. Its
  ;; measured TWA changes with wind; only the compass command must be integral.
  (call-with-fine-test
   (lambda (routing)
     (multiple-value-bind (sample endpoint)
         (fine-sail-step routing (routing-start routing) (fine-time+ (routing-starttime routing) 30d0)
                         120d0 '(:heading 107))
       (assert (string= (trackpoint-control-mode sample) "heading"))
       (assert (= (trackpoint-control-value sample) 107))
       (assert (> (abs (- (trackpoint-twa sample) (round (trackpoint-twa sample)))) 0.1d0))
       (let ((end (copy-trackpoint sample)))
         (setf (trackpoint-time end) (fine-time+ (trackpoint-time sample) 120d0)
               (trackpoint-position end) endpoint)
         (assert (fine-integer-plan-p (list sample end))))))
   t)
  (format t "~&Fixed-heading fitting, integer commands, eligibility and physical-TWA regressions passed.~%"))
