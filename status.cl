;;; Runtime settings and memory snapshots for the Status tab.
(in-package :bitsailor)

(defstruct statusparameter group name value)
(defstruct forecastcachegroup source date run resolution entries bytes)
(defstruct memorystatus heap-used heap-limit forecast-bytes forecast-entries groups land-cache)
(defstruct landcachestatus enabled limit-bytes estimated-bytes payload-bytes entries tiles hits misses evictions fallbacks)
(defstruct cachestatus polars combined-polars races)

(defun status-grid-resolution (info)
  "Describe actual GRIB longitude/latitude increments in degrees, without filename hints."
  (when info
    (let ((dx (cl-weather::gribinfo-i-inc info)) (dy (cl-weather::gribinfo-j-inc info)))
      (when (and (plusp dx) (plusp dy))
        (flet ((decimal (value)
                 (string-right-trim "." (string-right-trim "0" (format nil "~,6f" value)))))
          (if (< (abs (- dx dy)) 1d-9) (decimal dx)
              (format nil "~a x ~a" (decimal dx) (decimal dy))))))))

(defun status-file-resolution (path)
  "Read only the first GRIB message's grid header, without decoding forecast arrays."
  (ignore-errors
    (cl-eccodes:with-c-file (file (namestring path) "rb")
      (unless (cffi:null-pointer-p file)
        (let ((handle (cl-eccodes:codes-grib-handle-new-from-file file)))
          (unless (cffi:null-pointer-p handle)
            (unwind-protect
                 (status-grid-resolution (cl-weather::read-message-info handle))
              (cl-eccodes:codes-handle-delete handle))))))))

(defun download-grid-resolution (id timestamp)
  "Read an available file from the displayed download cycle; unknown grids remain unknown."
  (when timestamp
    (ignore-errors
      (let* ((source (cl-weather::get-datasource id (cl-weather::make-cycle% :timestamp timestamp)))
             (paths (remove-duplicates
                      (mapcar (lambda (step) (cl-weather::local-pathname source step))
                              (cl-weather::schedule-steps (cl-weather::schedule source))) :test #'equal)))
        (loop for path in paths
              thereis (and (probe-file path) (status-file-resolution path)))))))

;;; Escape configuration text, including quotes, backslashes and control characters, for JSON.
(defun write-status-json-string (stream value)
  (write-char #\" stream)
  (loop :for char :across value :do
    (case char
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (t (if (< (char-code char) 32)
             (format stream "\\u~4,'0x" (char-code char))
             (write-char char stream)))))
  (write-char #\" stream))

;;; Serialize settings without relying on the legacy generic string writer's escaping.
(defmethod json% (stream (parameter statusparameter))
  (write-string "{\"group\":" stream)
  (write-status-json-string stream (statusparameter-group parameter))
  (write-string ",\"name\":" stream)
  (write-status-json-string stream (statusparameter-name parameter))
  (write-string ",\"value\":" stream)
  (write-status-json-string stream (statusparameter-value parameter))
  (write-char #\} stream))

;;; Return the current values of operational configuration knobs, including local overrides.
;;; The explicit inventory excludes mutable working state, locks, account data and credentials.
(defun get-router-status-parameters ()
  (loop :for (group name unit) :in
    '(("Routing" "*max-angle*" "degrees either side of target bearing")
      ("Routing" "*max-iso-points*" "points per stage")
      ("Routing" "*max-route-hours*" "hours")
      ("Routing" "*twa-steps*" "degrees; NIL uses polar samples")
      ("Routing" "*search-box-margin-factor*" "fraction of route length")
      ("Routing" "*search-box-min-margin*" "minimum latitude padding, degrees")
      ("Routing" "*use-bitmap*" "bitmap land checks")
      ("Routing" "*tracks*" "alternative track output")
      ("Gates" "*gate-bucket-width*" "degrees, minimum bucket width")
      ("Gates" "*gate-origin-distance-factor*" "gate widths behind midpoint; minimum two")
      ("Gates" "*gate-coverage-bins*" "intervals per gate")
      ("Gates" "*gate-retirement-coverage*" "fraction of intervals required")
      ("Fine tuning" "*fine-tune-enabled*" "Permit requested TWA post-processing (requests default off)")
      ("Fine tuning" "*fine-tune-outward-enabled*" "outward anchors and max_duration progress objective")
      ("Fine tuning" "*fine-tune-approximation-enabled*" "broader same-tack approximation pass")
      ("Fine tuning" "*fine-tune-approximation-limit*" "approximation lateral corridor, metres")
      ("Fine tuning" "*fine-tune-approximation-evaluations*" "reserved approximation trial budget")
      ("Fine tuning" "*fine-tune-rolling-enabled*" "overlapping windows with retained incumbents")
      ("Fine tuning" "*fine-tune-rolling-horizons*" "rolling horizons, seconds")
      ("Fine tuning" "*fine-tune-rolling-stride*" "additional rolling starts, seconds")
      ("Fine tuning" "*fine-tune-rolling-position-limit*" "rolling lateral corridor, metres")
      ("Fine tuning" "*fine-tune-rolling-window-evaluations*" "base trial integrations per rolling window")
      ("Fine tuning" "*fine-tune-mark-tolerance*" "requested mark touch tolerance, metres")
      ("Fine tuning" "*fine-tune-heading-enabled*" "fixed-heading fits, including gradual turns in the broad pass")
      ("Fine tuning" "*fine-tune-heading-evaluations*" "additional heading trials per eligible window")
      ("Fine tuning" "*fine-tune-step*" "maximum trial integration step, seconds")
      ("Fine tuning" "*fine-tune-window*" "shooting-window duration, seconds")
      ("Fine tuning" "*fine-tune-angle-limit*" "degrees beyond original local TWA range")
      ("Fine tuning" "*fine-tune-position-limit*" "lateral/behind corridor bound, metres")
      ("Fine tuning" "*fine-tune-time-limit*" "manoeuvre time displacement, seconds")
      ("Fine tuning" "*fine-tune-endpoint-tolerance*" "endpoint closure tolerance, metres")
      ("Fine tuning" "*fine-tune-fit-tolerance*" "internal shooting closure, metres")
      ("Fine tuning" "*fine-tune-max-window-evaluations*" "trial integrations per window")
      ("Fine tuning" "*fine-tune-max-evaluations*" "trial integrations per route")
      ("Fine tuning" "*fine-tune-weather-cache-limit*" "maximum trial-time parameter-cache entries")
      ("Fine tuning" "*fine-tune-max-seconds*" "wall-clock trial budget, seconds")
      ("Point approaches" "*approach-max-step*" "seconds")
      ("Point approaches" "*approach-max-iterations*" "iterations")
      ("Point approaches" "*approach-lookback*" "predecessors")
      ("Legacy helpers" "*gate-samples*" "sample intervals; unused by shared-clock gates")
      ("Services" "*disable-nmea*" "NMEA disabled")
      ("Services" "*mailhost*" "activation mail host")
      ("Services" "*mailuser*" "activation mail user")
      ("Paths" "*rcfile*" "configuration file setting")
      ("Paths" "*server-config*" "HTTP configuration")
      ("Paths" "*source-root*" "source directory")
      ("Paths" "*web-root-directory*" "web directory")
      ("Paths" "*polars-dir*" "polars directory")
      ("Paths" "*races-dir*" "race definitions directory")
      ("Paths" "*db*" "database file"))
    :for symbol = (find-symbol (string-upcase name) :bitsailor)
    :collect (make-statusparameter :group group :name name
               :value (if (and symbol (boundp symbol))
                          (format nil "~a (~a)"
                                  (let ((value (symbol-value symbol)))
                                    (typecase value
                                      (pathname (or (ignore-errors (namestring value))
                                                    (namestring (make-pathname :name :wild :defaults value))))
                                      (ratio (format nil "~a (~,1f%)" value (* 100d0 value)))
                                      (t value))) unit)
                          "Unavailable"))))

(defun get-status-parameters ()
  "Include CL-MAP cache controls alongside the router's operational settings."
  (append (get-router-status-parameters)
    (loop for name in '("*PREPARED-CACHE-ENABLED*" "*PREPARED-CACHE-MAX-BYTES*"
                       "*PREPARED-CACHE-MAX-ENTRIES*" "*PREPARED-CACHE-CELL-SIZE*"
                       "*PREPARED-CACHE-TILE-BYTES*" "*PREPARED-CACHE-MAX-TILES*"
                       "*PREPARED-CACHE-MAX-QUERY-TILES*")
          collect (make-statusparameter :group "Land cache" :name (format nil "cl-map:~(~a~)" name)
                    :value (format nil "~a" (symbol-value (find-symbol name :cl-map)))))))

(defun get-land-cache-status ()
  "Snapshot the shared native-cache budget without traversing geometries or holding forecast locks."
  (let ((s (cl-map:prepared-cache-statistics)))
    (make-landcachestatus :enabled (getf s :enabled) :limit-bytes (getf s :limit-bytes)
      :estimated-bytes (getf s :estimated-bytes) :payload-bytes (getf s :payload-bytes)
      :entries (getf s :entries) :tiles (getf s :tiles) :hits (getf s :hits) :misses (getf s :misses)
      :evictions (getf s :evictions) :fallbacks (getf s :fallbacks))))

;;; Count unique decoded forecast-array payloads under the cache lock, grouped by source/cycle.
;;; Heap usage is an instantaneous SBCL measurement and can include reclaimable garbage;
;;; polling this endpoint never forces a collection. Native allocations are excluded.
(defun get-memory-status ()
  (let ((groups (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'eq))
        (bytes 0) (entries 0))
    (bordeaux-threads:with-lock-held (cl-weather::+forecast-ht-lock+)
      (setf entries (hash-table-count cl-weather::*forecast-ht*))
      (maphash
       (lambda (key forecast)
         (let* ((resolution (status-grid-resolution
                             (typecase forecast
                               (cl-weather::uv (cl-weather::uv-info forecast))
                               (cl-weather::gribmessage (cl-weather::gribmessage-info forecast)))))
                (group-key (list (second key) (fourth key) (third key) resolution))
                (group (or (gethash group-key groups)
                           (setf (gethash group-key groups)
                             (make-forecastcachegroup :source (second key) :date (fourth key)
                               :run (third key) :resolution resolution :entries 0 :bytes 0)))))
           (incf (forecastcachegroup-entries group))
           (dolist (array (typecase forecast
                           (cl-weather::uv (list (cl-weather::uv-u-array forecast)
                                                 (cl-weather::uv-v-array forecast)))
                           (cl-weather::gribmessage (list (cl-weather::gribmessage-data forecast)))))
             (when (and array (not (gethash array seen)))
               (setf (gethash array seen) t)
               (let ((size (* (array-total-size array)
                              (cond ((equal (array-element-type array) 'double-float) 8)
                                    ((equal (array-element-type array) 'single-float) 4)
                                    (t 0)))))
                 (incf bytes size)
                 (incf (forecastcachegroup-bytes group) size))))))
       cl-weather::*forecast-ht*))
    (make-memorystatus :heap-used #+sbcl (sb-kernel:dynamic-usage) #-sbcl nil
                      :heap-limit #+sbcl (sb-ext:dynamic-space-size) #-sbcl nil
                      :forecast-bytes bytes :forecast-entries entries :land-cache (get-land-cache-status)
                      :groups (loop :for group :being :the :hash-values :of groups :collect group))))

;;; Count raw polar IDs, combined option-specific polar tables and loaded race records.
(defun get-cache-status ()
  (make-cachestatus :polars (hash-table-count *polars-id-ht*)
    :combined-polars (loop :for options :being :the :hash-values :of *combined-polars-ht*
                          :sum (hash-table-count options))
    :races (let ((symbol (find-symbol "*RACES-HT*" :bitsailor)))
             (if (and symbol (boundp symbol)) (hash-table-count (symbol-value symbol)) 0))))
