;;; Run from Bitsailor with SBCL/Quicklisp loaded; see README.md in this directory.
;;; Isolated diagnostic process: real expansion/pruning, synthetic constant wind and no land.
(ql:quickload "bitsailor")
(load "tests/gate-search-tests.cl")
(in-package :bitsailor)

;;; Export post-retirement frontier snapshots without changing the search engine.
(defun capture-gate-frontier ()
  (let* ((start (parse-datetime "2026-10-31T00:00:00Z"))
         (grid (cl-weather::make-gribinfo :lat-start 1d0 :lat-end -1d0 :lat-points 3
                 :lon-start 0d0 :lon-end 360d0 :lon-points 361 :i-inc 1d0 :j-inc 1d0))
         (field (cl-weather::make-uv :cycle start :info grid
                  :u-array (make-array 1083 :element-type 'double-float :initial-element 0d0)
                  :v-array (make-array 1083 :element-type 'double-float :initial-element -10d0)))
         (params (cl-weather::make-params :fc0 field :fc1 field))
         (polars (make-cpolars :label "constant-speed demonstration"
                   :maxspeed (make-array '(1801 101) :initial-element '(5d0 0))
                   :vmg (make-array 101 :initial-element
                          (list (make-vmg :twa 45d0) (make-vmg :twa 135d0)))))
         (angles (coerce (loop :for angle :from 45 :to 135 :by 5 :collect (float angle 1d0))
                         '(vector double-float)))
         (routing (make-routing :start (make-latlng :lat 0d0 :lng 0d0)
                    :dest (make-latlng :lat 0d0 :lng 0.14d0)
                    :forecast-model 'cl-weather:noaa-gfs-wind :starttime start :stepmax 3600
                    :polars polars :twa-angles (make-twa-angles-buffer (make-cpolars :twa angles))))
         (gate (make-gate :a (make-latlng :lat 0.02d0 :lng 0.04d0)
                          :b (make-latlng :lat 0.12d0 :lng 0.04d0)))
         (original (symbol-function 'record-gate-coverage))
         (frames nil)
         (*gate-retirement-coverage* 1/2))
    (call-with-gate-test-functions
     (list (cons 'get-params (lambda (&rest arguments) (declare (ignore arguments)) params))
           (cons 'approach-blocked-p (lambda (&rest arguments) (declare (ignore arguments)) nil))
           (cons 'record-gate-coverage
                 (lambda (states gates coverage)
                   (funcall original states gates coverage)
                   (when states
                     (push (list
                             (round (timestamp-difference (routepoint-time (gate-state-point (first states))) start))
                             (coerce (aref coverage 0) 'list)
                             (loop :for state :in states
                                   :unless (gate-stage-retired-p (gate-state-stage state) coverage)
                                     :collect (let ((position (routepoint-position (gate-state-point state)))
                                                    (origin (routepoint-position (gate-state-origin state))))
                                                (list (gate-state-stage state)
                                                      (latlng-lng position) (latlng-lat position)
                                                      (latlng-lng origin) (latlng-lat origin)))))
                           frames)))))
     (lambda ()
       (let ((info (get-route-through-gates routing (list gate))))
         (format t "~&Demonstration: ~a, ~a~%" (routeinfo-status info) (routeinfo-stats info)))))
    (with-open-file (out "figures/gate-frontier/frontiers.json" :direction :output :if-exists :supersede)
      ;; Keep geographic precision for checking fan boundaries in the exported data.
      (write-char #\[ out)
      (loop :for (seconds bins states) :in (nreverse frames)
            :for first-frame = t :then nil :do
        (unless first-frame (write-char #\, out))
        (format out "[~d,[~{~d~^,~}],[" seconds bins)
        (loop :for (stage lon lat origin-lon origin-lat) :in states
              :for first-point = t :then nil :do
          (unless first-point (write-char #\, out))
          (format out "[~d,~,10f,~,10f,~,10f,~,10f]" stage lon lat origin-lon origin-lat))
        (write-string "]]" out))
      (write-char #\] out))))
(capture-gate-frontier)
