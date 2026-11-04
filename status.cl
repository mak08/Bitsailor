;;; Runtime settings and memory snapshots for the Status tab.
(in-package :bitsailor)

(defstruct statusparameter group name value)
(defstruct forecastcachegroup source date run resolution entries bytes)
(defstruct memorystatus heap-used heap-limit forecast-bytes forecast-entries groups)
(defstruct cachestatus polars combined-polars races)

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
(defun get-status-parameters ()
  (loop :for (group name unit) :in
    '(("Routing" "*max-angle*" "degrees either side of target bearing")
      ("Routing" "*max-iso-points*" "points per stage")
      ("Routing" "*max-route-hours*" "hours")
      ("Routing" "*twa-steps*" "degrees; NIL uses polar samples")
      ("Routing" "*use-bitmap*" "bitmap land checks")
      ("Routing" "*tracks*" "alternative track output")
      ("Gates" "*gate-bucket-width*" "degrees, minimum bucket width")
      ("Gates" "*gate-origin-distance-factor*" "gate widths behind midpoint; minimum two")
      ("Gates" "*gate-coverage-bins*" "intervals per gate")
      ("Gates" "*gate-retirement-coverage*" "fraction of intervals required")
      ("Fine tuning" "*fine-tune-enabled*" "TWA post-processing enabled")
      ("Fine tuning" "*fine-tune-outward-enabled*" "outward anchors and max_duration progress objective")
      ("Fine tuning" "*fine-tune-approximation-enabled*" "broader same-tack approximation pass")
      ("Fine tuning" "*fine-tune-approximation-limit*" "approximation lateral corridor, metres")
      ("Fine tuning" "*fine-tune-approximation-evaluations*" "reserved approximation trial budget")
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
         (let* ((group-key (list (second key) (fourth key) (third key) (fifth key)))
                (group (or (gethash group-key groups)
                           (setf (gethash group-key groups)
                             (make-forecastcachegroup :source (second key) :date (fourth key)
                               :run (third key) :resolution (fifth key) :entries 0 :bytes 0)))))
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
                      :forecast-bytes bytes :forecast-entries entries
                      :groups (loop :for group :being :the :hash-values :of groups :collect group))))

;;; Count raw polar IDs, combined option-specific polar tables and loaded race records.
(defun get-cache-status ()
  (make-cachestatus :polars (hash-table-count *polars-id-ht*)
    :combined-polars (loop :for options :being :the :hash-values :of *combined-polars-ht*
                          :sum (hash-table-count options))
    :races (let ((symbol (find-symbol "*RACES-HT*" :bitsailor)))
             (if (and symbol (boundp symbol)) (hash-table-count (symbol-value symbol)) 0))))
