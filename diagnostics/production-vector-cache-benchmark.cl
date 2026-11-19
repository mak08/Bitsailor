;;; Load after Bitsailor, configure polars/maps/forecasts, then call the helper.
(in-package :bitsailor)

(defun benchmark-production-vector-cache (routing &optional (stream *standard-output*))
  "Compare production indexed/prepared modes without hooks or relaxed land checks.
Run in an isolated process to avoid unrelated load. Forward search only; exact
best-track and isochrone JSON must agree across all four runs."
  (when (or (routing-waypoints routing) (routing-gate routing) (routing-reverse-percent routing))
    (error "This benchmark isolates single-leg forward routing"))
  (let ((reference nil) (results nil))
    (dolist (case '((:indexed-first nil) (:indexed-warm nil) (:prepared-first t) (:prepared-warm t)))
      (let* ((cl-map:*prepared-cache-enabled* (second case))
             (*routing-wave-state* (make-hash-table :test #'equal))
             (request (copy-routing routing)))
        (setf (routing-twa-angles request) (copy-seq (routing-twa-angles routing)))
        (let* ((info (get-route-leg request)) (stats (routeinfo-stats info))
               (row (list :mode (first case) :seconds (routestats-calctime stats)
                          :points (routestats-points stats)
                          :positions-per-second (/ (routestats-points stats) (routestats-calctime stats)))))
          (if reference
              (progn
                (assert (= (routestats-points (routeinfo-stats reference)) (routestats-points stats)))
                (assert (equal (with-output-to-string (s) (json s (routeinfo-best reference)))
                               (with-output-to-string (s) (json s (routeinfo-best info)))))
                (assert (equal (with-output-to-string (s) (json s (routeinfo-isochrones reference)))
                               (with-output-to-string (s) (json s (routeinfo-isochrones info))))))
              (setf reference info))
          (push row results) (format stream "~&~s~%" row) (finish-output stream))))
    (format stream "~&~s~%" (cl-map:prepared-cache-statistics))
    (nreverse results)))
