;;; Load into a running Bitsailor image, then call (router::report-routing-memory).
;;; No cache entries are removed. Full GC is optional because it pauses the image.
(in-package :bitsailor)

;;; Report SBCL heap usage and decoded forecast-array bytes by source/cycle/resolution.
;;; Return numeric totals without retaining forecast objects. Array payload sizes exclude
;;; object headers, cache keys, native allocations and non-specialized array storage.
(defun report-routing-memory (&key (stream *standard-output*) (gc nil))
  (when gc (sb-ext:gc :full t))
  (let ((groups (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'eq))
        (cache-bytes 0) (entries 0))
    (bordeaux-threads:with-lock-held (cl-weather::+forecast-ht-lock+)
      (setf entries (hash-table-count cl-weather::*forecast-ht*))
      (maphash
       (lambda (key forecast)
         (let* ((group (list (second key) (fourth key) (third key) (fifth key)))
                (totals (or (gethash group groups)
                            (setf (gethash group groups) (list 0 0)))))
           (incf (first totals))
           (dolist (array (typecase forecast
                           (cl-weather::uv (list (cl-weather::uv-u-array forecast)
                                                 (cl-weather::uv-v-array forecast)))
                           (cl-weather::gribmessage (list (cl-weather::gribmessage-data forecast)))))
             (when (and array (not (gethash array seen)))
               (setf (gethash array seen) t)
               (let ((bytes (* (array-total-size array)
                               (cond ((equal (array-element-type array) 'double-float) 8)
                                     ((equal (array-element-type array) 'single-float) 4)
                                     (t 0)))))
                 (incf cache-bytes bytes)
                 (incf (second totals) bytes))))))
       cl-weather::*forecast-ht*))
    (let ((heap-used (sb-kernel:dynamic-usage))
          (heap-limit (sb-ext:dynamic-space-size)))
      (format stream "~&Heap: ~,1f / ~,1f MiB ~:[(GC not requested)~;(after full GC)~]~%"
              (/ heap-used 1048576d0) (/ heap-limit 1048576d0) gc)
      (format stream "Forecast cache: ~d entries, ~,1f MiB unique decoded array payload~%"
              entries (/ cache-bytes 1048576d0))
      (maphash (lambda (group totals)
                 (destructuring-bind (source date run resolution) group
                   (format stream "  ~a ~a/~2,'0d ~a: ~d entries, ~,1f MiB~%"
                           source date run resolution (first totals)
                           (/ (second totals) 1048576d0)))) groups)
      (list :heap-used heap-used :heap-limit heap-limit :forecast-entries entries
            :forecast-array-bytes cache-bytes))))
