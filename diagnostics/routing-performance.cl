;;; Load after Bitsailor with polars/maps/forecast directories configured.
;;; This does not start HTTP, download forecasts, or change production settings.
(in-package :bitsailor)

(defun benchmark-routing-performance (routing &key (repetitions 2) (include-fine-tuning t)
                                                   (stream *standard-output*))
  "Measure this single-leg route with/without waves, separating search and tuning.
The first execution warms weather and coastline caches. Returns measurement plists.
Use an explicit ROUTING-CYCLE and locally available forecasts for repeatability."
  (when (or (routing-waypoints routing) (routing-gate routing) (routing-reverse-percent routing))
    (error "This benchmark isolates single-leg forward routing"))
  (let ((results nil))
    (labels ((measure (waves phase repeat)
               (let* ((request (copy-routing routing))
                      (*routing-wave-state* (make-hash-table :test #'equal)))
                 (setf (routing-use-waves request) waves
                       (routing-twa-angles request) (copy-seq (routing-twa-angles routing)))
                 (let* ((info (get-route-leg request))
                        (search-seconds (routestats-calctime (routeinfo-stats info)))
                        (points (routestats-points (routeinfo-stats info)))
                        (started (get-internal-real-time)))
                   (when (eq phase :full) (fine-tune-route request info))
                   (let* ((tuning-seconds (if (eq phase :full)
                                             (/ (- (get-internal-real-time) started)
                                                (float internal-time-units-per-second 1d0)) 0d0))
                          (row (list :waves waves :phase phase :repeat repeat
                                     :points points :search-seconds search-seconds
                                     :tuning-seconds tuning-seconds
                                     :search-positions-per-second (/ points search-seconds)
                                     :reported-positions-per-second
                                     (/ points (routestats-calctime (routeinfo-stats info))))))
                     (push row results)
                     (format stream "~&~s~%" row)
                     (finish-output stream))))))
      (dotimes (repeat repetitions)
        (dolist (waves '(nil t)) (measure waves :search repeat)))
      (when include-fine-tuning
        (dolist (waves '(nil t)) (measure waves :full repetitions))))
    (nreverse results)))
