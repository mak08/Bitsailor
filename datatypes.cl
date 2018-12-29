;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-12-29 16:46:43>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Time

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
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)
          (timestamp-second timestamp :timezone timezone)))

(defun parse-datetime (string)
  (destructuring-bind (date time)
      (cl-utilities:split-sequence #\T string)
    (destructuring-bind (y m d)
        (cl-utilities:split-sequence #\- date)
      (destructuring-bind (utime &optional (tz ""))
          (cl-utilities:split-sequence #\Z time)
        (destructuring-bind (hr min &optional (sec "0"))
            (cl-utilities:split-sequence #\: utime)
          (assert (equal tz ""))
          (encode-timestamp 0
                            (read-from-string sec)
                            (read-from-string min)
                            (read-from-string hr)
                            (read-from-string d)
                            (read-from-string m)
                            (read-from-string y)
                            :timezone +utc-zone+))))))

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
