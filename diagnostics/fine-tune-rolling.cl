;;; Load Bitsailor and configure forecasts/polars/maps as for a normal request,
;;; then load this file and call (router::benchmark-fine-tune-rolling routing).
;;; Search once, compare all tuners against the identical unmodified result.
(in-package :bitsailor)

;;; Isolate budget, corridor and rolling-horizon effects without changing weather
;;; or the original search. Return an alist of named ROUTEINFO results for review.
(defun benchmark-fine-tune-rolling (routing &optional (stream *standard-output*))
  (let ((raw (let ((*fine-tune-enabled* nil)) (get-route routing))) (results nil))
    (dolist (config '((baseline nil 3000 10d0 500d0 (3600d0))
                      (budget-only nil 12000 30d0 500d0 (3600d0))
                      (rolling-tight t 12000 30d0 500d0 (14400d0 7200d0 3600d0 1800d0))
                      (rolling-hour t 12000 30d0 9260d0 (3600d0))
                      (rolling-wide t 12000 30d0 9260d0 (14400d0 7200d0 3600d0 1800d0))))
      (destructuring-bind (name enabled budget seconds corridor horizons) config
        (let* ((*fine-tune-rolling-enabled* enabled)
               (*fine-tune-max-evaluations* budget) (*fine-tune-max-seconds* seconds)
               (*fine-tune-rolling-position-limit* corridor) (*fine-tune-rolling-horizons* horizons)
               (info (copy-routeinfo raw)) (started (get-internal-real-time)))
          (fine-tune-route routing info)
          (let ((report (routeinfo-fine-tuning info)))
            (format stream "~&~a: ~,3fs, commands ~a -> ~a, ~a trials, ~a/~a rolling windows, ETA ~a, distance ~,3fm~%"
                    name (/ (- (get-internal-real-time) started) internal-time-units-per-second)
                    (fine-tuning-original-changes report) (fine-tuning-optimized-changes report)
                    (fine-tuning-evaluations report) (fine-tuning-rolling-improvements report)
                    (fine-tuning-rolling-windows report) (fine-tuning-optimized-arrival report)
                    (fine-tuning-optimized-destination-distance report)))
          (push (cons name info) results))))
    (nreverse results)))
