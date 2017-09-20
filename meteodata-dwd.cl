;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Download, process and provide DWD ICON wind data
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-20 21:26:34>

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
;;; Variables

(defvar *dwd-forecast-bundle* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions

(defclass dwd-icon-bundle (forecast-bundle)
  ((dwd-icon-data :reader dwd-icon-data :initarg :data)))

(defmethod get-forecast-bundle ((datasource (eql 'dwd-icon-bundle)))
  *dwd-forecast-bundle*)
  
(defmethod fcb-time ((bundle dwd-icon-bundle))
  (let ((grib (dwd-icon-data bundle)))
    (gribfile-forecast-time grib)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non API functions

(defmethod update-forecast-bundle ((bundle (eql 'dwd-icon-bundle)) &key (resolution "0.250") (timestamp (now)))
  (let ((timestamp (update-local-files :timestamp timestamp :resolution resolution)))
    (cond
      ((and *dwd-forecast-bundle* (timestamp<=  timestamp (fcb-time *dwd-forecast-bundle*)))
       (log2:info "Forecast data for ~a already loaded" timestamp))
      (t
       (let ((grib (read-dwd-wind-data timestamp :resolution resolution)))
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
             (download-dwd-file u-file)
             (download-dwd-file v-file)
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

(defun download-dwd-file (filename
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
;;; Loading GRIB2 files

(defun read-dwd-wind-data (timestamp &key (resolution "0.250") (directory *grib-folder*))
  "Load U_10M and V_10M data from the DWD ICON files specified by timestamp and resolution"
  (let* ((u10-path (format nil "~a/~a" directory (grib-file-name "U_10M" timestamp :resolution resolution)))
         (v10-path (format nil "~a/~a" directory (grib-file-name "V_10M" timestamp :resolution resolution)))
         (index (grib-index-new '("step" "shortName"))))
    
    (log2:info "Add file ~a  => ~a~%" u10-path (grib-index-add-file index u10-path))
    (log2:info "Add file ~a  => ~a~%" v10-path (grib-index-add-file index v10-path))

    (get-values-from-index index)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
