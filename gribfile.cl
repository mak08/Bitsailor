;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-11-01 17:04:38>

(in-package :virtualhelm)


(defun get-values-from-index (index)
  (let* ((steps 
          (codes-index-get-long index "step"))
         (shortNames
          (codes-index-get-string index "shortName"))
         (forecasts
          (make-array (length steps)))
         (result (make-gribfile)))
    (loop
       :for i ::from 0
       :for step :across steps
       :do (progn
             (codes-index-select-long index "step" step)
             (let ((u-array
                    (progn 
                      (codes-index-select-string index "shortName" "10u")
                      (with-handle-from-index (handle index)
                        (when (= i 0)
                          (get-grid-info result handle)
                          (log2:info "Processing ~a messages [~ax~a]"
                                     (length steps)
                                     (gribfile-lat-points result)
                                     (gribfile-lon-points result)))
                        (codes-get-double-array handle "values"))))
                   (v-array
                    (progn 
                      (codes-index-select-string index "shortName" "10v")
                      (with-handle-from-index (handle index)
                        (codes-get-double-array handle "values")))))
               (setf (aref forecasts i)
                     (make-grib-values :forecast-time (* step 60)
                                       :u-array u-array
                                       :v-array v-array)))))
    (codes-index-delete index)
    (setf (gribfile-data result) forecasts)
    (values
     result)))

(defun get-grid-info (gribfile message)
  (let* ((date (format nil "~a" (codes-get-long message "dataDate")))
         (time (format nil "~4,,,'0@a" (codes-get-long message "dataTime")))
         (dummy (log2:info "Reading grib with date=~a time=~a" date time))
         (timestamp (parse-timestring
                     (format () "~a-~a-~aT~a:~a:00+00:00"
                             (subseq date 0 4)
                             (subseq date 4 6)
                             (subseq date 6 8)
                             (subseq time 0 2)
                             (subseq time 2 4))))
         (forecast-time (codes-get-long message "forecastTime")))
    (setf (gribfile-forecast-time gribfile) (adjust-timestamp timestamp (offset :hour forecast-time))
          (gribfile-grid-size gribfile) (codes-get-size message "values")
          (gribfile-step-units gribfile) (codes-get-long message "stepUnits")
          (gribfile-lat-points gribfile) (codes-get-long message "numberOfPointsAlongAMeridian")
          (gribfile-lon-points gribfile) (codes-get-long message "numberOfPointsAlongAParallel")
          (gribfile-lat-start gribfile) (coerce (codes-get-long message "latitudeOfFirstGridPointInDegrees") 'double-float)
          (gribfile-lat-end gribfile) (coerce (codes-get-long message "latitudeOfLastGridPointInDegrees") 'double-float)
          (gribfile-lon-start gribfile) (coerce (codes-get-long message "longitudeOfFirstGridPointInDegrees") 'double-float)
          (gribfile-lon-end gribfile) (coerce (codes-get-long message "longitudeOfLastGridPointInDegrees") 'double-float)
          (gribfile-j-inc gribfile) (codes-get-double message "jDirectionIncrementInDegrees") ; "south to north"
          (gribfile-i-inc gribfile) (codes-get-double message "iDirectionIncrementInDegrees") ; "west to east"
          (gribfile-j-scan-pos gribfile) (codes-get-long message "jScansPositively")
          (gribfile-i-scan-neg  gribfile) (codes-get-long message "iScansNegatively"))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
