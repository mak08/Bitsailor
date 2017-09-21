;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-21 21:16:42>

(in-package :virtualhelm)


(defun get-values-from-index (index)
  (let* ((steps 
          (grib-index-get-long index "step"))
         (shortNames
          (grib-index-get-string index "shortName"))
         (forecasts
          (make-array (length steps)))
         (result (make-gribfile)))
    (loop
       :for i ::from 0
       :for step :across steps
       :do (progn
             (grib-index-select-long index "step" step)
             (let ((u-array
                    (progn 
                      (grib-index-select-string index "shortName" "10u")
                      (with-handle-from-index (handle index)
                        (when (= i 0)
                          (get-grid-info result handle)
                          (log2:info "Processing ~a messages [~ax~a]"
                                     (length steps)
                                     (gribfile-lat-points result)
                                     (gribfile-lon-points result)))
                        (grib-get-double-array handle "values"))))
                   (v-array
                    (progn 
                      (grib-index-select-string index "shortName" "10v")
                      (with-handle-from-index (handle index)
                        (grib-get-double-array handle "values")))))
               (setf (aref forecasts i)
                     (make-grib-values :forecast-time (* step 60)
                                       :u-array u-array
                                       :v-array v-array)))))
    (grib-index-delete index)
    (setf (gribfile-data result) forecasts)
    (values
     result)))

(defun get-grid-info (gribfile message)
  (let* ((date (format nil "~a" (grib-get-long message "dataDate")))
         (time (format nil "~4,,,'0a" (grib-get-long message "dataTime")))
         (dummy (log2:info "Reading grib with timestamp <~aT~a>" date time))
         (timestamp (parse-timestring
                     (format () "~a-~a-~aT~a:~a:00+00:00"
                             (subseq date 0 4)
                             (subseq date 4 6)
                             (subseq date 6 8)
                             (subseq time 0 2)
                             (subseq time 2 4))))
         (forecast-time (grib-get-long message "forecastTime")))
    (setf (gribfile-forecast-time gribfile) (adjust-timestamp timestamp (offset :hour forecast-time))
          (gribfile-grid-size gribfile) (grib-get-size message "values")
          (gribfile-step-units gribfile) (grib-get-long message "stepUnits")
          (gribfile-lat-points gribfile) (grib-get-long message "numberOfPointsAlongAMeridian")
          (gribfile-lon-points gribfile) (grib-get-long message "numberOfPointsAlongAParallel")
          (gribfile-lat-start gribfile) (coerce (grib-get-long message "latitudeOfFirstGridPointInDegrees") 'double-float)
          (gribfile-lat-end gribfile) (coerce (grib-get-long message "latitudeOfLastGridPointInDegrees") 'double-float)
          (gribfile-lon-start gribfile) (coerce (grib-get-long message "longitudeOfFirstGridPointInDegrees") 'double-float)
          (gribfile-lon-end gribfile) (coerce (grib-get-long message "longitudeOfLastGridPointInDegrees") 'double-float)
          (gribfile-j-inc gribfile) (grib-get-double message "jDirectionIncrementInDegrees") ; "south to north"
          (gribfile-i-inc gribfile) (grib-get-double message "iDirectionIncrementInDegrees") ; "west to east"
          (gribfile-j-scan-pos gribfile) (grib-get-long message "jScansPositively")
          (gribfile-i-scan-neg  gribfile) (grib-get-long message "iScansNegatively"))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
