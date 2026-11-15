;;; Load after Bitsailor, then call TEST-STATUS. No weather or map files are needed.
(in-package :bitsailor)

;;; Verify shared-array accounting, source grouping, runtime overrides and JSON escaping.
(defun test-status ()
  (let* ((cl-weather::*forecast-ht* (make-hash-table :test #'equalp))
         (array (make-array 10 :element-type 'double-float :initial-element 0d0))
         (wave (make-array 5 :element-type 'double-float :initial-element 0d0))
         (uv (cl-weather::make-uv :u-array array :v-array array
               :info (cl-weather::make-gribinfo :i-inc 0.027779d0 :j-inc 0.027779d0)))
         (*gate-retirement-coverage* 1/2))
    (setf (gethash '(0 "test-wind" 0 "20261031" "0p25") cl-weather::*forecast-ht*) uv
          (gethash '(3 "test-wind" 0 "20261031" "0p25") cl-weather::*forecast-ht*) uv
          (gethash '(0 "test-wave" 0 "20261031" "0p50") cl-weather::*forecast-ht*)
          (cl-weather::make-gribmessage :data wave))
    (let ((memory (get-memory-status)))
      (assert (= 120 (memorystatus-forecast-bytes memory)))
      (assert (= 3 (memorystatus-forecast-entries memory)))
      (assert (= 2 (length (memorystatus-groups memory))))
      ;; The cache key says 0p25, but the decoded regional grid has finer spacing.
      (assert (equal "0.027779" (forecastcachegroup-resolution
                                 (find "test-wind" (memorystatus-groups memory)
                                       :key #'forecastcachegroup-source :test #'string=))))
      (assert (null (forecastcachegroup-resolution
                     (find "test-wave" (memorystatus-groups memory)
                           :key #'forecastcachegroup-source :test #'string=)))))
    (assert (search "50.0%"
                    (statusparameter-value
                     (find "*gate-retirement-coverage*" (get-status-parameters)
                           :key #'statusparameter-name :test #'string=))))
    (assert (every (lambda (parameter)
                     (not (string= "Unavailable" (statusparameter-value parameter))))
                   (get-status-parameters))))
  (let ((encoded (with-output-to-string (stream)
                   (write-status-json-string stream (format nil "~s~%~c" "name" #\\)))))
    (assert (search "\\\"name\\\"" encoded))
    (assert (search "\\U000A" (string-upcase encoded)))
    (assert (search "\\\\" encoded)))
  (assert (equal "0.25" (status-grid-resolution
                         (cl-weather::make-gribinfo :i-inc 0.25d0 :j-inc 0.25d0))))
  (assert (equal "0.1 x 0.2" (status-grid-resolution
                              (cl-weather::make-gribinfo :i-inc 0.1d0 :j-inc 0.2d0))))
  (format t "~&Status memory accounting and runtime parameter tests passed.~%"))
