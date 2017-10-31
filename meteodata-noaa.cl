;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-10-31 00:49:16>

(in-package :virtualhelm)
;; (declaim (optimize speed (debug 0) (space 0) (safety 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving NOAA wind forecasts
;;;    Model: NOAA GFS
;;;    Resolution: 1 degree
;;;    GRIB2 times are UTC
;;;    NOAA GFS forecasts are produced every 6hrs (four cycles per day)
;;;
;;; Forecast availability
;;;    Cycle nn becomes available at nn+5hrs UTC (eg. cycle 18 at 23:00Z, 01:00MEST(+1d)
;;;
;;; VR forecast usage
;;;    0700 UTC Update: Cycle 00z, timestamp H06
;;;    1900 UTC Update: Cycle 12z, timestamp H06


(defvar *noaa-forecast-bundle* nil)

;;; Offset (in minutes) of the forecast used at a given time
(defparameter +noaa-offset+ 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions

(defclass noaa-bundle (forecast-bundle)
  ((noaa-data :reader noaa-data :initarg :data)))

(defmethod get-forecast-bundle ((datasource (eql 'noaa-bundle)))
  *noaa-forecast-bundle*)

(defmethod fcb-time ((bundle noaa-bundle))
  (let ((grib (noaa-data bundle)))
    (gribfile-forecast-time grib)))

(defmethod fcb-max-offset ((bundle noaa-bundle))
  (let ((grib (noaa-data bundle)))
    (* (fcb-stepwidth bundle)
       (1- (length (gribfile-data grib))))))

(defclass noaa-forecast (forecast)
  ((noaa-bundle :reader noaa-bundle :initarg :bundle)
   (fc-time :reader fc-time :initarg :time)))

(defmethod get-forecast ((bundle noaa-bundle) (utc-time local-time:timestamp))
  (make-instance 'noaa-forecast :bundle bundle :time utc-time))

(defmethod get-wind-forecast ((forecast noaa-forecast) latlng)
  (let* ((bundle
          (noaa-bundle forecast)) 
         (grib
          (noaa-data bundle))
         (offset
          ;; NOAA forecasts are 3-hourly
          ;; ... but interpolated to 1 hour steps
          (- (truncate
              (/ (timestamp-difference (fc-time forecast) (fcb-time bundle))
                 (* (fcb-stepwidth bundle) 3600)))
             +noaa-offset+)))
    (get-interpolated-wind grib offset (latlng-lat latlng) (latlng-lng latlng))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update data

(defmethod update-forecast-bundle ((bundle (eql 'noaa-bundle)) &key (stepwidth 3))
  ;; If we don't have files, or if newer files than we have should be available,
  ;; update to the latest available files.
  (let ((timestamp (when *noaa-forecast-bundle* (fcb-time *noaa-forecast-bundle*))))
    (log2:info "Updating NOAA forecast from ~a" timestamp)
    ;; Forecast is outdated, fetch new. The latest available cycle will should be 4:30
    (let* ((new-timestamp (adjust-timestamp (now) (offset :hour -5)))
           (date (format-timestring nil new-timestamp :format '((:year 4) (:month 2) (:day 2))))
           (cycle (* 6 (truncate (timestamp-hour new-timestamp) 6))))
      (log2:info "At ~a, looking for cycle ~a-~a" (now) date  cycle)
      (tagbody
        :start
        (let* ((filenames (download-noaa-bundle date cycle))
               (numfiles (length filenames)))
          (when (and (>= numfiles 1)
                     (or (null *noaa-forecast-bundle*)
                         (>= numfiles 41)))
            (let ((data (read-noaa-wind-data filenames)))
              (ecase stepwidth
                (1
                 (setf data (insert-interpolated-forecasts data)))
                (3))
              (setf *noaa-forecast-bundle*
                    (make-instance 'noaa-bundle
                                   :data data
                                   :stepwidth stepwidth))))
          (when (< numfiles 81)
            (log2:info "Have ~a files, waiting 10min" numfiles)
            (sleep 600)
            (go :start))
          (log2:info "Done"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download forecasts from NOAA.
;;;    See http://nomads.ncep.noaa.gov/
;;;
;;; Example URL:
;;;    http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_1p00.pl?file=gfs.t12z.pgrb2.1p00.f000&lev_10_m_above_ground=on&var_UGRD=on&var_VGRD=on&leftlon=0&rightlon=360&toplat=90&bottomlat=-90&dir=%2Fgfs.2017091712

(defun download-noaa-bundle (date cycle)
  ;; Date: yyyymmdd 
  ;; Cycle: 00|06|12|18
  ;; 3hourly forecasts will be downloaded (offsets 0..240)
  (ecase cycle ((or 0 6 12 18)))
  (loop
     :for k :from 1
     :for offset :from 0 :to 240 :by 3
     :for file = (ignore-errors
                   (download-noaa-file date cycle offset))
     :while file
     :collect file :into files
     :finally (return (values files
                              cycle))))

(defun noaa-spec-and-destfile (date &key (cycle "0") (offset 6) (basename "pgrb2") (resolution "1p00"))
  (let* ((directory
          (format () "~a~2,,,'0@a" date cycle))
         (spec
          (format () "gfs.t~2,,,'0@az.~a.~a.f~3,,,'0@a" cycle basename resolution offset))
         (destfile
          (format () "~a_~a.grib2" date spec)))
    (values directory spec destfile)))

(defun download-noaa-file (date cycle offset)
  (multiple-value-bind (directory spec destfile)
      (noaa-spec-and-destfile date :cycle cycle :offset offset)
    (let ((destpath (format () "~a/~a" *grib-folder* destfile)))
      (if (probe-file destpath)
          (log2:info "File exists: ~a(~a:~a))" destpath date cycle)
          (progn
            (log2:info "~a(~a:~a)" destpath date cycle)
            (multiple-value-bind
                  (out error-out status)
                (download-noaa-file% directory spec destfile)
              (case status
                (0
                 (let ((download-size
                        (with-open-file (f (format () "~a/~a" *grib-folder* destfile))
                          (file-length f))))
                   (when (< download-size 90000)
                     (uiop:delete-file-if-exists destpath)
                     (error "Short file. Forecast ~a:~a not available yet?" date spec))))
                (otherwise
                 (error "cURL error ~a" status))))))
      destpath)))

(defun download-noaa-file% (directory spec destfile &key  (resolution "1p00"))
  "Retrieve the GRIB file valid at timestamp according to VR rules"
  (let* ((dest-folder
          *grib-folder*)
         (url
          (concatenate 'string
                       "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_" resolution ".pl?"
                       "file=" spec
                       "&dir=%2Fgfs." directory
                       "&lev_10_m_above_ground=on"
                       "&var_UGRD=on"
                       "&var_VGRD=on"
                       "&leftlon=0"
                       "&rightlon=360"
                       "&toplat=90"
                       "&bottomlat=-90"))
         (ftp-command
          (format () "curl -n \"~a\" -o ~a/~a" url dest-folder destfile)))
    (uiop:run-program ftp-command)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessing GRIB wind data

(defun read-noaa-wind-data (filenames)
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (when (null filenames)
    (error "No input files"))
  (let ((index (grib-index-new '("step" "shortName"))))
    (dolist (filename filenames)
      (log2:info "Add file ~a~%" filename)
      (grib-index-add-file index filename))
    (get-values-from-index index)))


(defun insert-interpolated-forecasts (gribfile)
  (let* ((forecasts (gribfile-data gribfile))
         (interpolated-forecasts
          (make-array (* (1- (length forecasts)) 3))))
    (loop
       :for i :below (1- (length forecasts))
       :do (progn
             (setf (aref interpolated-forecasts (* i 3))
                   (aref forecasts i))
             (setf (aref interpolated-forecasts (+ (* i 3) 1))
                   (interpolate-forecast (aref forecasts i) (aref forecasts (1+ i)) (/ 1 3)))
             (setf (aref interpolated-forecasts (+ (* i 3) 2))
                   (interpolate-forecast (aref forecasts i) (aref forecasts (1+ i)) (/ 2 3)))))
    (setf (gribfile-data gribfile)
          interpolated-forecasts)
    gribfile))

(defun interpolate-forecast (t0 t1 fraction)
  (let* ((t0-u (grib-values-u-array t0))
         (t0-v (grib-values-v-array t0))
         (t1-u (grib-values-u-array t1))
         (t1-v (grib-values-v-array t1))
         (dimensions (array-dimensions t0-u))
         (result-u (make-array dimensions :element-type 'double-float))
         (result-v (make-array dimensions :element-type 'double-float)))
    (log2:info "dim:~a t=~a"
               dimensions
               (+ (grib-values-forecast-time t0)
                                        (* fraction (- (grib-values-forecast-time t1)
                                                       (grib-values-forecast-time t0)))))
    (loop
       :for u0 :across t0-u
       :for u1 :across t1-u
       :for v0 :across t0-v
       :for v1 :across t1-v
       :for i :from 0
       :do (setf (aref result-u i)
                 (+ u0 (* fraction (- u1 u0)))
                 (aref result-v i)
                 (+ v0 (* fraction (- v1 v0)))))
    (make-grib-values :forecast-time (+ (grib-values-forecast-time t0)
                                        (* fraction (- (grib-values-forecast-time t1)
                                                       (grib-values-forecast-time t0))))
                      :u-array result-u
                      :v-array result-v)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
