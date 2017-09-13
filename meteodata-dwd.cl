;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Download, process and provide DWD ICON wind data
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-13 20:57:21>

(in-package :virtualhelm)


(declaim (optimize (speed 3) (safety 1) (debug 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overview
;;;
;;; Download
;;;     GRIB2 files are downloaded asynchonously in the specified resolution from separate files for U and V data.
;;;
;;; Processing
;;;     U and V data are assembled and stored in a hashtable by timestamp. Old data is periodically deleted.
;;;
;;; Providing data
;;;     Data is found by timestamp only, in the resolution that was downloaded and stored.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resources
;;; http://www.dwd.de/SharedDocs/downloads/DE/modelldokumentationen/nwv/icon/icon_dbbeschr_future.pdf?view=nasPublication
;;; https://www.dwd.de/SharedDocs/downloads/DE/modelldokumentationen/nwv/gme/gme_dbbeschr_201405.pdf?__blob=publicationFile&v=5
;;; https://software.ecmwf.int/wiki/display/GRIB/Documentation
;;; 
;;; cat ~/.netrc
;;; curl -n ftp://ftp-outgoing2.dwd.de/gds/ICON/grib/europe/ICON_GDS_europe_reg_0.250x0.250_U_10M_2017040300.grib2.bz2 -o ICON_GDS_europe_reg_0.250x0.250_U_10M_2017040300.grib2.bz2 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinate Systems
;;;
;;; Google Maps
;;;    Latitude:    0..90  from Equator to North Pole
;;;                 -90..0 from South Pole to Equator
;;;              == 90..-90 from North Pole to South Pole   
;;;    Longitude:   0..180 from Greenwich Meridien to date line
;;;                 -180..0 from date line to Greenwich Meridien
;;;
;;; GRIB (usually; if i-scans-negatively=0 and j-scans-positively = 1)
;;;    Latitude:    90..-90 from North Pole to South Pole
;;;    Longitude:   0..359 from Greenwich Meridien to EAST 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar *grib-folder* "/home/michael/Wetter/dwd-gds")
(defvar *forecasts* (make-hash-table :hash-function #+sbcl #'timestamp-to-universal #+ccl 'timestamp-to-universal))
(defvar *dwd-forecast-bundle* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions


(defclass dwd-icon-bundle (forecast-bundle)
  ((dwd-icon-data :reader dwd-icon-data :initarg :data)))

(defmethod get-forecast-bundle ((datasource (eql 'dwd-icon-bundle)))
  *dwd-forecast-bundle*)
  
(defmethod fcb-time ((bundle dwd-icon-bundle))
  (let ((grib (dwd-icon-data bundle)))
    (grib-filespec-date (gribfile-timespec grib))))

(defclass dwd-icon-forecast (forecast)
  ((dwd-icon-bundle :reader dwd-icon-bundle :initarg :bundle)
   (fc-time :reader fc-time :initarg :time)))


(defmethod get-forecast ((bundle dwd-icon-bundle) (utc-time local-time:timestamp))
  (make-instance 'dwd-icon-forecast :bundle bundle :time utc-time))

(defmethod get-wind-forecast ((forecast dwd-icon-forecast) latlng)
  (let* ((bundle
          (dwd-icon-bundle forecast)) 
         (grib
          (dwd-icon-data bundle))
         (offset
          (truncate
           (/ (timestamp-difference (fc-time forecast) (fcb-time bundle))
              3600))))  
    (get-interpolated-wind grib offset (latlng-lat latlng) (latlng-lng latlng))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Periodic updates

(defun start-grib-updates (&key (resolution "0.250"))
  "Check for new forecast data an remove old data periodically"
  (flet ((run-updates ()
           (loop
              (update-forecast-data :resolution resolution)
              (sleep 3600))))
    (bordeaux-threads:make-thread #'run-updates :name "FORECASTS-UPDATER")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non API functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast offset

(defun gribfile-offset (gribfile &optional (timestamp (now)))
  "Compute the offset for accessing wind data for $timestamp in $gribfile."
  (let ((now (adjust-timestamp timestamp (set :nsec 0) (set :sec 0) (set :minute 0))))
    (/ (timestamp-difference now (grib-filespec-date (gribfile-timespec gribfile))) 3600)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpolation
;;; - Speed
;;;   Compute speed at GRIB coordinates, then interpolate at intermediate coordinates
;;; - Direction
;;;   Interpolate U and V componentes at intermediate coordinates, then calculate direction (U/V interpolation)

(defun get-interpolated-wind (grib offset lat lon)
  "Get interpolated direction(deg) & speed(m/s) at lat, lon.
Data for offset (hour) is used and no time interpolation is done."
  (declare (double-float lat lon))

  (when (< lon 0d0)
    (incf lon 360d0))
  
  (handler-case
      (let ((i-inc (gribfile-i-inc grib))
            (j-inc (gribfile-j-inc grib)))
        (declare (double-float  i-inc j-inc))
        (with-bindings
            (((flat rlat) (ffloor lat j-inc))
             ((flon rlon) (ffloor lon i-inc)))
          (cond
            ((and (eql rlat 0d0)
                  (eql rlon 0d0))
             ;; Don't interpolate if requested coordinates fall on a grid node
             (with-bindings (((u v) (get-wind grib offset lat lon)))
               (values (angle u v)
                       (enorm u v))))
            (t
             (let*
                 ((x0 (* (ffloor lon i-inc) i-inc))
                  (x1 (+ x0 i-inc))
                  (y0 (* (ffloor lat j-inc) j-inc))
                  (y1 (+ y0 j-inc)))
               (declare (double-float x0 x1 y0 y1))
               (with-bindings (((u00 v00) (get-wind grib offset y0 x0))
                               ((u01 v01) (get-wind grib offset y0 x1))
                               ((u10 v10) (get-wind grib offset y1 x0))
                               ((u11 v11) (get-wind grib offset y1 x1)))
                 (declare (double-float x0 x1 y0 y1 u00 u01 u10 u11 v00 v10 v01 v11))
                 (let* ((u (bilinear lon lat x0 x1 y0 y1 u00 u01 u10 u11))
                        (v (bilinear lon lat x0 x1 y0 y1 v00 v01 v10 v11))
                        (s (bilinear lon lat x0 x1 y0 y1
                                     (enorm u00 v00)
                                     (enorm u01 v01)
                                     (enorm u10 v10)
                                     (enorm u11 v11))))
                   (values (angle u v)
                           s))))))))
    (error (e)
      (log2:error "Could not retrieve interpolated wind: ~a~%" e)
      (values 0d0 -1d0))))

(defun update-forecast-data (&key (resolution "0.250") (timestamp (now)))
  (let ((timestamp (update-local-files :timestamp timestamp :resolution resolution)))
    (cond
      ((timestamp<= (fcb-time *dwd-forecast-bundle*))
       (log2:info "Forecast data for ~a already loaded" timestamp))
      (t
       (let ((grib (read-icon-wind-data timestamp :resolution resolution)))
         (setf *dwd-forecast-bundle*
               (make-instance 'dwd-icon-bundle :data grib))))))
  (values t))

;; Get spec for the newest GRIB file providing data for $time+$offset, from the latest cycle that encompasses $time"
;; Avaliability of forecasts: T+5h (T=0,6,12,18) UTC
;; See http://www.dwd.de/DE/leistungen/gds/help/inhalt_allgemein/gds_content_de_en_pdf.pdf?__blob=publicationFile&v=7
(defun update-local-files (&key (resolution "0.250") (timestamp (now)))
  (setf timestamp (normalize-timestamp timestamp))
  (flet ((try-file (timestamp)
           ;; GRIB time is divisible by 6
           (let ((u-file (grib-file-name "U_10M" timestamp :resolution resolution))
                 (v-file (grib-file-name "V_10M" timestamp :resolution resolution)))
             (log2:trace "Trying ~a~%"
                         (format-timestring () timestamp :format '((:year 4) (:month 2) (:day 2) (:hour 2)) :timezone +utc-zone+))
             (download-file u-file)
             (download-file v-file)
             ;; Successful download - return timestamp
             (values timestamp))))
    (handler-case
        (try-file timestamp)
      (error ()
        (adjust-timestamp! timestamp (offset :hour -6))
        (try-file timestamp)))))

(defun normalize-timestamp (timestamp)
  (multiple-value-bind (q r) (truncate (timestamp-hour timestamp :timezone +utc-zone+) 6)
    (adjust-timestamp! timestamp (offset :hour (- r))))
  (adjust-timestamp! timestamp (set :nsec 0) (set :sec 0) (set :minute 0)))

(defun grib-file-name (variable timestamp &key (map "world") (resolution "0.250"))
  (let ((timestamp (format-timestring () timestamp :format '((:year 4) (:month 2) (:day 2) (:hour 2)) :timezone +utc-zone+)))
    (values
     (format nil "ICON_GDS_~a_reg_~ax~:*~a_~a_~a.grib2" map resolution variable timestamp))))

(defun download-file (filename
                      &key
                        (dest-file filename)
                        (path "gds/ICON/grib/")
                        (map-folder "world/")
                        (dest-path *grib-folder*)
                        (server "ftp-outgoing2.dwd.de"))
  (cond
    ((probe-file (format nil "~a/~a" dest-path dest-file))
     (log2:info "File ~a already downloaded~%" dest-file))
    (t
     (let ((ftp-command
            (format () "curl -n ftp://~a/~a~a~a.bz2 -o ~a/~a.bz2" server path map-folder filename dest-path dest-file))
           (bunzip2-command
            (format () "bunzip2 ~a/~a.bz2" dest-path dest-file)))
       (log2:info "Downloading ~a~%" dest-file)
       (uiop:run-program ftp-command)
       (log2:info "Unpacking ~a~%" dest-file)
       (uiop:run-program bunzip2-command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessing GRIB wind data
(declaim (ftype (function (t integer double-float double-float) (values double-float double-float)) get-wind))

(defun get-wind (grib offset lat lon)
  "Get u/v values at lat,lon (in GRIB coordinates)"
  (declare (double-float lat lon))
  (declare (ftype (function (array) (simple-array double-float)) grib-values-u-array grib-values-v-array))
  (let
      ;; Get grib parameters to check if latlon is within grib range.
      ;; Also used for computing index into data array
      ((lat0 (gribfile-lat-start grib))
       (lon0 (gribfile-lon-start grib))
       (lat1 (gribfile-lat-end grib))
       (lon1 (gribfile-lon-end grib))
       (latpoints (gribfile-lat-points grib))
       (lonpoints (gribfile-lon-points grib))
       ;; Use forecast data at offset (hour). No time interpolation.
       (grib-values
        (aref (gribfile-data grib) offset)))
    (declare (double-float lat0 lat1 lon0 lon1))
    (declare ((unsigned-byte 16) lonpoints latpoints))
    ;; 0 - Check if latlon is within grib coordinates
    (unless (and (<= lat0 lat lat1)
                 (or (and (< lon0 lon1)
                          (<= lon0 lon lon1))
                     (or (<= lon0 lon 360)
                         (<= 0 lon lon1))))
      (error "Invalid coordinates [~a, ~a]" lat lon))
    (let
        ;; Compute values array index
        ;; 1 - offset by grib boundaries
        ((olat (- lat lat0))
         (olon (- lon lon0)))
      ;; 2 - Normalize to Median/Parallel ranges (0-180, 0-360)
      (when (< olat 0) (incf olat 180))
      (when (< olon 0) (incf olon 360))
      ;; 3 - Divide by grid step width to obtain array index
      (let* ((i-inc (gribfile-i-inc grib))
             (j-inc (gribfile-j-inc grib))
             (lat-index (floor olat j-inc))
             (lon-index (floor olon i-inc)))
        (declare (double-float i-inc j-inc))
        (declare ((unsigned-byte 16) lat-index lon-index))
        ;; Paranoia
        (unless (<= 0 lon-index lonpoints)
          (error "Invalid data index ~a for lon ~a in ~a" lon-index lon grib-values))
        (unless (<= 0 lat-index latpoints)
          (error "Invalid data index ~a for lat ~a in ~a" lon-index lon grib-values))
        ;; 4 - Return grib data
        ;; (log2:trace "Getting latlon[~a,~a] at index[~a,~a]~%" lat lon lat-index lon-index)
        (let* ((lat-offset (* lat-index lonpoints))
               (offset (+ lat-offset lon-index)))
          (values (aref (the (simple-array double-float) (grib-values-u-array grib-values))
                        offset)
                  (aref (the (simple-array double-float) (grib-values-v-array grib-values))
                        offset)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading GRIB2 files

(defun read-icon-wind-data (timestamp &key (resolution "0.250") (directory *grib-folder*))
  "Load U_10M and V_10M data from the DWD ICON files specified by timestamp and resolution"
  (let* ((u10-path (format nil "~a/~a" directory (grib-file-name "U_10M" timestamp :resolution resolution)))
         (v10-path (format nil "~a/~a" directory (grib-file-name "V_10M" timestamp :resolution resolution)))
         (index (grib-index-new '("step" "shortName"))))
    
    (log2:info "Add file ~a  => ~a~%" u10-path (grib-index-add-file index u10-path))
    (log2:info "Add file ~a  => ~a~%" v10-path (grib-index-add-file index v10-path))

    (let* ((steps 
            (grib-index-get-long index "step"))
           (shortNames
            (grib-index-get-string index "shortName"))
           (forecasts
            (make-array (length steps)))
           (result (make-gribfile :timespec (make-grib-filespec :date timestamp))))
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
       result))))

(defun get-grid-info (gribfile message)
  (setf (gribfile-forecast-time gribfile) (grib-get-long message "forecastTime")
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
        (gribfile-i-scan-neg  gribfile)(grib-get-long message "iScansNegatively")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
