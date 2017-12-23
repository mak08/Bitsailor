;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-12-23 01:46:07>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit conversion

(defun knots-to-m/s (knots)
  (coerce (* 463 (/ knots 900)) 'double-float))

(defun m/s-to-knots (m/s)
  (coerce (* 900 (/ m/s 463)) 'double-float))

(defun m/s-to-kM/H (m/s)
  (* m/s 3.6))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - HEADING values range from 0..360 (clockwise from north)
;;; - ANGLE  values range from -179..180 (starboard downwind to port downwind)  

(deftype heading () `(integer 0 360))
(deftype angle () `(double-float -179.99999d0 180.0d0))

(defun normalize-heading (value)
  (if (> value 360)
      (- value 360)
      (if (< value 0)
          (+ value 360)
          value)))

(defun normalize-angle (value)
  (if (<= value -180)
      (+ value 360)
      (if (> value 180)
          (- value 360)
          value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structs

;; TODO: A latlng should only be used to represent Google Maps coordinates.
(defstruct latlng
  (lat 0 :read-only t)
  (lng 0 :read-only t)
  latr%
  lngr%)
(defmethod print-object ((thing latlng) stream)
  (format stream "[~3$, ~3$]" (latlng-lat thing) (latlng-lng thing)))

(defun latlng-latr (latlng)
  (or (latlng-latr% latlng)
      (setf (latlng-latr% latlng)
            (rad (latlng-lat latlng)))))

(defun latlng-lngr (latlng)
  (or (latlng-lngr% latlng)
      (setf (latlng-lngr% latlng)
            (rad (latlng-lng latlng)))))

(defstruct dms (degrees 0) (minutes 0) (seconds 0))

(defun dms2decimal (dms)
  (+ (dms-degrees dms)
     (/ (dms-minutes dms) 60)
     (/ (dms-seconds dms) 3600)))

(defun decimal2dms (dec)
  (multiple-value-bind (d r1)
      (truncate dec)
    (multiple-value-bind (m r2)
        (truncate (* r1 60))
      (make-dms :degrees d :minutes m :seconds (* r2 60)))))

(defstruct gribfile
  "Basic / common GRIB data"
  forecast-time                 ; The timestamp (yyyymmdd, hh) of the forecast (identical with the first forecast in the bundle)
  grid-size                     ; Number of data points, should equal lat-points * lon-points
  step-units
  lat-start lat-end lat-points  ; Start, end and number of points along parallel
  lon-start lon-end lon-points  ; Start, end and number of points along meridian
  i-inc j-inc
  i-scan-neg j-scan-pos
  data                          ; Array of forecast data for successive forecast times
  )

(defmethod print-object ((thing gribfile) stream)
  (format stream "{gribfile <~a,~a>/<~a,~a> ~a}"
          (gribfile-lat-start thing)
          (gribfile-lon-start thing)
          (gribfile-lat-end thing)
          (gribfile-lon-end thing)
          (gribfile-forecast-time thing)))

(defstruct grib-filespec
  region
  resolution
  date)

(defun format-timespec-datehh (stream timestamp &key (timezone +utc-zone+) (offset 0))
  "Format timestamp as YYYY-MM-DD, HH Uhr"
  (format-timestring stream
                     (adjust-timestamp timestamp (offset :hour offset))
                     :format '((:year 4) "-" (:month 2) "-" (:day 2) ", " (:hour 2) "Uhr") :timezone timezone))  

(defun format-time (stream timestamp)
  (format stream "~2,'0d:~2,'0dZ"
          (timestamp-hour timestamp :timezone +utc-zone+)
          (timestamp-minute timestamp :timezone +utc-zone+)))

(defun format-date (stream timestamp)
  (format stream "~4,'0d-~2,'0d-~2,'0d"
          (timestamp-year timestamp :timezone +utc-zone+)
          (timestamp-month timestamp :timezone +utc-zone+)
          (timestamp-day timestamp :timezone +utc-zone+)))

(defun format-datetime (stream timestamp &key (timezone +utc-zone+))
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)))

(defmethod print-object ((thing timestamp) stream)
  (format-rfc1123-timestring stream thing))

(defstruct grib-values
  forecast-time                 ; 
  u-array
  v-array
  vmax-data)

(defmethod print-object ((thing grib-values) stream)
  (format stream "{grib-values ~a ~a}"
          (grib-values-forecast-time thing)
          (array-dimensions
           (grib-values-u-array thing))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
