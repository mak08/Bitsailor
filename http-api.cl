;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2023-03-05 22:49:03>


(in-package :bitsailor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP API

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic handlers

(defun get-page (server handler request response)
  (handler-case 
      (let* ((user-id (http-authenticated-user handler request))
             (app (get-request-app request))
             (race-id (get-routing-request-race-id request))
             (path (merge-pathnames (make-pathname :name app :type "html")
                                    *web-root-directory*)))
        (log2:info "race-id: ~a" race-id)
        (setf (http-header response :|Content-Location|)
              (path request))
        (load-file path response))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;; This function is called from a dynamic handler
(defun start-page (server handler request response)
  (handler-case 
      (let* ((path
               (merge-pathnames (make-pathname :name "startpage" :type "html")
                                *web-root-directory*)))
        (load-file path response))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;; This function is called from a dynamic handler
(defun router (server handler request response)
  (handler-case 
      (let* ((user-id (http-authenticated-user handler request))
             (app (get-request-app request))
             (race-id (get-routing-request-race-id request))
             (race-info (race-info race-id))
             (page-base-name
               (typecase race-info
                 (race-info-rs "router-rs")
                 (race-info-vr "router-vr")
                 (null
                  (error "Unknown race ~a" race-id))))
             (path (merge-pathnames (make-pathname :name page-base-name :type "html")
                                    *web-root-directory*))
             (query (parameters request)))
        (log2:info "race-id: ~a" race-id)
        (log2:info "type: ~a" (type-of (race-info race-id)))
        (setf (http-header response :|Content-Location|)
              (path request))
        (load-html-file path response :substitutions (list (cons "GOOGLE_API_KEY" *api-key*))))
    (error (e)
      (log2:error "~a" e)
      (setf (http-header response "Connection") "Close")
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e))
      (setf (http-body response)
            (format nil "<html><body>~a</body></html>" e)))))

;;; This function is called from a dynamic handler
(defun activate-account (server handler request response)
  (sql:with-connection (*dbcon*)
    (let* ((secret (cadr (path request)))
           (provisional (get-user-prov-by-secret secret)))
      (cond
        ((null provisional)
         (setf (http-body response)
               (format nil "This link is invalid or has expired. Please re-register your e-mail address.")))
        (t
         (sql:?delete 'bitsailor.user_prov
                      :where (sql:?= (sql:?upper 'email)
                                     (string-upcase (email provisional))))
         (add-user (email provisional)
                   (pwhash provisional)
                   (boatname provisional)
                   (status provisional))
         (let ((activated
                 (merge-pathnames (make-pathname :name "activated" :type "html")
                                  (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                                                    '("web"))))))
           (load-file activated response)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User sign-up

(defun |signUp| (handler request response &key |emailAddress| |boatName| |password|)
  (declare (ignore handler request))
  (sql:with-connection (*dbcon*)
    (let* ((email (decode-uri-component |emailAddress|))
           (boat (decode-uri-component |boatName|))
           (password |password|)
           (link-secret (create-uuid)))
      (log2:info "~a ~a" email boat)
      (multiple-value-bind (success message)
          (register-signup email link-secret boat password)
        (cond
          (success
           (send-email email link-secret boat)
           "Activation e-mail sent")
          (t
           (setf (status-code response) 400)
           (case message
             (:boatname-exists
              (format nil "~a is already taken" boat))
             (othwerwise
              "An error occurred"))))))))

(defun decode-uri-component (s)
  (let ((*read-base* 16))
    (do
     ((k 0)
      (result (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
     ((>= k (length s))
      result)
      (cond
        ((eql (aref s k) #\%)
         (vector-push-extend (code-char (read-arg (subseq s (1+ k) (+ k 3)))) result)
         (incf k 3))
        (t
         (vector-push-extend (aref s k) result)
         (incf k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getRoute

(defun determine-minwind (presets race-id)
  (when (string= presets "VR")
    (if (string=
         (joref (race-info-data (race-info race-id)) "fineWinds")
         "TRUE")
        (knots-to-m/s 1d0)
        (knots-to-m/s 2d0))))

(defun get-routing-presets (presets
                            &key
                              (race-id)
                              (starttime nil)
                              (resolution "1p00")
                              (polars)
                              (gfs-mode "06h")
                              (options)
                              (stepmax (* 24 60 60))
                              (cycle)
                              (slat)
                              (slon)
                              (dlat)
                              (dlon))
  (let* ((vr-finewinds
           (and (string= presets "VR")
                (string= "TRUE"
                         (joref (race-info-data (race-info race-id)) "fineWinds"))))
         (options
           (or options
               (if (string= presets "RS")
                   '("realsail")
                   '("hull" "foil" "winch" "heavy" "light" "reach"))))
         (polars (get-combined-polars polars (encode-options options))))
    (make-routing
     :race-id race-id
     :fan *max-angle*
     :start (when (and slat slon) (make-latlng :lat slat :lng slon))
     :dest  (when (and dlat dlon) (make-latlng :lat dlat :lng dlon))
     :starttime starttime
     :stepmax (min (* *max-route-hours* 3600) stepmax)
     :options options
     :resolution resolution
     :minwind (determine-minwind presets race-id)
     :gfs-mode gfs-mode
     :grib-source (if vr-finewinds :vr :noaa)
     :interpolation (cond
                      (vr-finewinds
                       :enorm)
                      ((string= presets "RS")
                       :bilinear)
                      (t
                       :vr))
     :polars polars
     :cycle cycle
     :merge-start  (if (string= presets "RS")
                       (get-rs-merge-delay cycle gfs-mode)
                       (if vr-finewinds
                           6d0
                           4d0))
     :merge-window (if (string= presets "RS")
                       0d0
                       (if vr-finewinds 0d0 2d0))
     :penalties  (if (string= presets "RS")
                     (make-penalty :sail 0.975d0 :tack 1d0 :gybe 1d0)
                     (make-penalty :sail 0.9375d0 :tack 0.9375d0 :gybe 0.85d0))
     :simplify-route (string= presets "RS"))))

(defun get-rs-merge-delay (cycle gfs-mode)
  (values
   (cond
     ((string= gfs-mode "06h")
      6d0)
     ((string= gfs-mode "12h")
      (case (cycle-run cycle)
        ((6 18)
         6d0)
        ((0 12)
         12d0)))
     (t
      (error "Unknown GFS mode ~a" gfs-mode)))
   cycle))

(defun |getRoute| (handler request response &key
                                              (|presets| "VR")
                                              (|raceId| nil)
                                              (|options| nil)
                                              (|resolution| "1p00")
                                              (|gfsMode| "06h")
                                              |polarsId|
                                              |slat|
                                              |slon|
                                              |dlat|
                                              |dlon|
                                              (|startTime| nil)
                                              (|cycleTS|  (determine-rs-cycle |gfsMode|) cycle-supplied-p)
                                              (|duration| (* *max-route-hours* 3600)
                                                          duration-supplied-p))
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (user-id
               (http-authenticated-user handler request))
             (cycle (if cycle-supplied-p
                        (make-cycle :timestamp (parse-datetime |cycleTS|))
                        |cycleTS|))
             (routing
               (get-routing-presets |presets|
                                    :race-id |raceId|
                                    :gfs-mode |gfsMode|
                                    :options (cl-utilities:split-sequence #\, |options|)
                                    :resolution |resolution|
                                    :polars |polarsId|
                                    :starttime |startTime|
                                    :cycle cycle
                                    :stepmax (if duration-supplied-p
                                                 (read-arg |duration|)
                                                 |duration|)
                                    :slat (read-arg |slat| 'double-float)
                                    :slon (read-arg |slon| 'double-float)
                                    :dlat (read-arg |dlat| 'double-float)
                                    :dlon (read-arg |dlon| 'double-float)))
             (routeinfo
               (get-route routing)))
        (log2:info "User:~a Race:(~a) Status ~a ~a"
                   user-id
                   |presets|
                   (routeinfo-status routeinfo)
                   (routeinfo-stats routeinfo))
        (values
         (with-output-to-string (s)
           (json s routeinfo))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

(defun vh:|getRouteRS| (handler request response &key
                                                (|polarsID| "aA32bToWbF")
                                                (|gfsMode| "06h")
                                                |latStart|
                                                |lonStart|
                                                |latDest|
                                                |lonDest|
                                                (|cycleTS| (available-cycle (now)) cycle-supplied-p)
                                                (|duration| (* 4 3600))
                                                (|resolution| "1p00"))
  (|getRouteRS| handler request response
                      :|polarsID| |polarsID|
                      :|gfsMode| |gfsMode|
                      :|latStart| |latStart|
                      :|lonStart| |lonStart|
                      :|latDest| |latDest|
                      :|lonDest| |lonDest|
                      :|cycleTS| |cycleTS|
                      :|duration| |duration|
                      :|resolution| |resolution|))

(defun |getRouteRS| (handler request response &key
                                                (|polarsID| "aA32bToWbF")
                                                (|gfsMode| "06h")
                                                |latStart|
                                                |lonStart|
                                                |latDest|
                                                |lonDest|
                                                (|cycleTS| (determine-rs-cycle |gfsMode|) cycle-supplied-p)
                                                (|duration| (* 4 3600))
                                                (|resolution| "1p00"))
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (user-id
               (http-authenticated-user handler request))
             (cycle (if cycle-supplied-p
                        (make-cycle :timestamp (parse-datetime |cycleTS|))
                        |cycleTS|))
             (routing
               (get-routing-presets "RS"
                                    :options '("realsail")
                                    :resolution |resolution|
                                    :polars |polarsID|
                                    :gfs-mode |gfsMode|
                                    :stepmax (min (* *max-route-hours* 3600) ;; 2d
                                                  (read-arg |duration|))
                                    :cycle cycle
                                    :slat (read-arg |latStart| 'double-float)
                                    :slon (read-arg |lonStart| 'double-float)
                                    :dlat (read-arg |latDest| 'double-float)
                                    :dlon (read-arg |lonDest| 'double-float)))
             (routeinfo
               (get-route routing)))
        (log2:info "User:~a Race:(RS) Status ~a ~a"
                   user-id
                   (routeinfo-status routeinfo)
                   (routeinfo-stats routeinfo))
        (values
         (with-output-to-string (s)
           (json s routeinfo))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

(defun determine-rs-cycle (gfs-mode)
  (let* ((cycle
           (available-cycle (now)))
         (run
           (cycle-run cycle)))
    (cond
      ((string= gfs-mode "12h")
       (if (or (eql run 0)
               (eql run 12))
           (previous-cycle cycle)
           cycle))
      ((string= gfs-mode "06h")
       cycle)
      (T
       (error "Unknown GFS mode ~a" gfs-mode)))))

(defun get-request-app (request)
  (let ((query-pairs (parameters request)))
    (or (cadr (assoc "app" query-pairs :test #'string=))
        "index")))

(defun get-routing-request-race-id (request)
  (or (cadr (assoc "race" (parameters request) :test #'string=))
      (let* ((referer (http-header request :|referer|))
             (query-string (puri:uri-query (puri:parse-uri referer)))
             (query-pairs (parse-url-query query-string)))
        (or (cadr (assoc "race" query-pairs :test #'string=))
            "default"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getWind

(defstruct forecast-data
  basetime                              ; Indicates the cycle
  maxoffset                             ; Max offset available in forecast data (384 for NOAA)
  cycle                                 ; Cycle in a nice format
  time                                  ; Forecast time
  data                                  ; Wind forecast values 
  )

;;; Get wind data in indicated (Google Maps) coordinates : [-90, 90], (-180,180].
;;; $|basetime|
;;;        Mandatory. Indcates the cycle that should be used.
;;; $|offset|
;;;        Optinonal. Requested time as an offset to  $|basetime|,
;;; $|time|
;;;        Optional. The requested time.
;;; Returns an error if the requested time is in the past or in the future of the requested cycle, or if $|offset| is too large.
;;; Returns (0d0, -1d0) for unavailable values.  Does not work if date line is in longitude range.
(defun |getWind| (handler request response &key (|presets|) (|cycle|) (|time|) (|resolution|) |north| |east| |west| |south| (|ddx| "0.5") (|ddy| "0.5") (|ySteps|) (|xSteps|))
  (declare (ignore |ySteps| |xSteps|))
  (log2:info "Cycle:~a Time:~a Res:~a N:~a S:~a W:~a E:~a" |cycle| |time| |resolution| |north| |south| |west| |east|)
  (assert (and |cycle| |time|))
  (handler-case
      (let* ((*read-default-float-format*
               'double-float)
             (cycle
               (make-cycle :timestamp (local-time:parse-rfc3339-timestring |cycle|)))
             (resolution
               |resolution|)
             (requested-time
               (local-time:parse-rfc3339-timestring |time|))
             (user-id
               (http-authenticated-user handler request))
             (race-id
               (get-routing-request-race-id request))
             (cycle-start-time (cycle-timestamp cycle))
             (ddx (read-arg |ddx|))
             (ddy (read-arg |ddy|))
             (north (read-arg |north|))
             (south (read-arg |south|))
             (east (read-arg |east|))
             (west (read-arg |west|)))
        (log2:info "DX: ~a DDX: ~a  DY: ~a DDY: ~a" (arc-length west east) ddx (- north south) ddy)
        (log2:info "Requested time: ~a Using cycle: ~a" requested-time cycle)
        (when (< west 0d0) (incf west 360d0))
        (when (< east 0d0) (incf east 360d0))
        (when (< east west) (incf east 360d0))
            
        (assert (and (plusp ddx)
                     (plusp ddy)
                     (< south north)))
        (let* ((wind-data
                 (get-wind-data |presets| cycle resolution requested-time north south west east ddx ddy))
               (body
                 (with-output-to-string (s)
                   (json s
                         (let ((time cycle-start-time))
                           (make-forecast-data
                            :basetime (format-datetime nil cycle-start-time)
                            :time (format-datetime nil requested-time)
                            :maxoffset 384 ;; (dataset-max-offset dataset)
                            :cycle (format-timestring nil
                                                      time
                                                      :format '((:year 4) "-" (:month 2) "-" (:day 2) "  " (:hour 2) "Z") :timezone local-time:+utc-zone+)
                            :data wind-data))))))
          (values body)))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))
    #+ccl(ccl::invalid-memory-access (e)
           (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
           (setf (status-code response) 500)
           (setf (status-text response) (format nil "~a" e)))))

(defun arc-length (a b)
  (when (< a 0) (incf a 360))
  (when (< b 0) (incf b 360))
  (when (< b a) (incf b 360))
  (- b a))

(defun get-wind-data (presets cycle resolution time north south west east ddx ddy)
  (let* ((routing
           (get-routing-presets presets))
         (iparams
           (interpolation-parameters time
                                     :method (routing-interpolation routing)
                                     :merge-start (routing-merge-start routing)
                                     :merge-duration (routing-merge-window routing)
                                     :cycle cycle
                                     :resolution resolution))
         (result (make-array (list (1+ (truncate (- north south) ddy))
                                   (1+ (truncate (- east west) ddx))
                                   2))))
    (loop
      :for lat :from north :downto south :by ddy
      :for ilat :from 0
      :do (loop
            :for lon :from west :to east :by ddx
            :for ilon :from 0
            :do (multiple-value-bind (dir speed)
                    (let ((nlon
                            (if (> lon 0) (- lon 360) lon)))
                      (interpolate lat nlon iparams))
                  (setf (aref result ilat ilon 0) (round-to-digits dir 2))
                  (setf (aref result ilat ilon 1) (round-to-digits speed 2)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wind Tiles

(defun parse-cycle-string (cycle)
  (make-cycle
   :timestamp (parse-datetime
               (format nil "~a-~a-~aT~a:00Z"
                       (subseq cycle 0 4)
                       (subseq cycle 4 6)
                       (subseq cycle 6 8)
                       (subseq cycle 9 11)))))

(defvar +wind-tile-lock+
    (bordeaux-threads:make-lock "wind-tile-lock-ht"))

(defvar *wind-tile-lock-ht*
  (make-hash-table :test 'equal))

(defun get-wind-tile-lock (path)
  (let ((name (namestring path)))
  (bordeaux-threads:with-lock-held (+wind-tile-lock+)
    (or (gethash name *wind-tile-lock-ht*)
        (setf (gethash name *wind-tile-lock-ht*)
              (bordeaux-threads:make-lock name))))))

(defun get-wind-tile (server handler request response)
  (handler-case 
      (destructuring-bind (tile cycle resolution offset lat lon)
          (path request)
        (let* ((cycle (parse-cycle-string cycle))
               (offset (read-arg offset 'fixnum))
               (lat (read-arg lat 'fixnum))
               (lon (read-arg (first (cl-utilities:split-sequence #\. lon)) 'fixnum))
               (path (tile-filename cycle resolution offset lat lon
                                    :tile-root-dir (merge-pathnames (make-pathname :directory '(:relative "tile"))
                                                                    *web-root-directory*))))
          (let ((lock (get-wind-tile-lock path)))
            (bordeaux-threads:with-lock-held (lock)
              (cond
                ((probe-file path)
                 (log2:trace "HIT: ~a" path))
                (t
                 (log2:trace "MISS: ~a" path)
                 (ensure-directories-exist path)
                 (create-tile path lat (+ lat 10) lon (+ lon 10)
                              :cycle cycle
                              :resolution resolution
                              :from-forecast offset
                              :to-forecast offset)))))
          (load-file path response)))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TWA Path

(defun |getTWAPath| (handler request response &key |presets| |raceId| |options| |gfsMode| |polarsId| |cycle| |time| |resolution| |latA| |lngA| |lat| |lng|)
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (user-id
               (http-authenticated-user handler request))
             (race-id (get-routing-request-race-id request))
             (cycle (make-cycle :timestamp (parse-timestring |cycle|)))
             (routing
               (get-routing-presets |presets|
                                    :race-id |raceId|
                                    :gfs-mode |gfsMode|
                                    :options (cl-utilities:split-sequence #\, |options|)
                                    :resolution |resolution|
                                    :polars |polarsId|
                                    :starttime |time|
                                    :cycle cycle))
             (time (parse-datetime |time|))
             (lat-a (read-arg |latA|))
             (lng-a (read-arg |lngA|))
             (lat (read-arg |lat|))
             (lng (read-arg |lng|)))
        (values
         (with-output-to-string (s)
           (json s (get-twa-path routing :cycle cycle :time time :lat-a lat-a :lng-a lng-a :lat lat :lng lng)))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Batch routing

(defun |checkWindow| (location request response &key (|logfile| "checkWindow.log"))
  (handler-case
      (let ((result
             (check-window  (local-time:adjust-timestamp (local-time:now) (:offset :hour 2))
                            (local-time:adjust-timestamp (local-time:now) (:offset :day 3))
                            :options '("winch" "foil" "heavy" "reach" "hull")
                            :logfile |logfile|)))
        (values
         (with-output-to-string (s)
           (json s result))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Race list  & race info

(defun |getRaceInfo| (handler request response)
    (let* ((user-id
             (http-authenticated-user handler request))
           (race-id
             (get-routing-request-race-id request))
           (race-info
             (race-info race-id)))
      (values
       (with-output-to-string (s)
         (json s race-info)))))

(defstruct raceinfo type name id gfs025 record class start-time closing-time start-pos finish-pos closed)

(defun |getRaceList| (handler request response)
  ;; unauthenticated!
  (declare (ignore handler request response))
  ;; (load-race-definitions :directory *races-dir*)
  (let ((races (list)))
    (map-races 
     (lambda (k v)
       (declare (ignore k))
       (push (get-raceinfo (race-info-data v)) races)))
    (with-output-to-string (s)
      (json s races))))

(defun get-raceinfo (race)
  (cond
    ((joref race "objectId")
     (make-raceinfo
      :type "rs"
      :name (joref race "name")
      :id (joref race "objectId")
      :gfs025 (ecase (joref race "gfs025")
                (false "no")
                (true "yes")
                ((nil) "(no)"))
      :record  (ecase (joref race "record")
                 (false "no")
                 (true "yes")
                 ((nil) "no"))
      :class (joref (joref race "polar") "classBoat")
      :start-time (joref (joref race "start") "iso")
      :start-pos (make-latlng
                  :lat (joref (joref race "startLocation") "latitude")
                  :lng (joref (joref race "startLocation") "longitude"))))
    ((joref race "_id")
     (let* ((id (joref race "_id"))
            (boat (joref race "boat"))
            (start (joref race "start")))
       (make-raceinfo
        :type "vr"
        :name (joref race "name")
        :id (format nil "~a.~a" (joref id "race_id") (joref id "num"))
        :gfs025 (if (string= (joref race "fineWinds") "TRUE") "yes" "no")
        :record (when (string= (joref (joref race "race") "type") "record") "yes")
        :class (joref boat "label")
        :start-time (joref start "date")
        :start-pos  (make-latlng
                     :lat (joref start "lat")
                     :lng (joref start "lon")))))))

;;; The web page can't fetch the position from the Telnet port itself.
(defun |getBoatPosition| (handler request response &key (|host| "nmea.realsail.net") (|port| ""))
  (unless *disable-nmea*
    (error "NMEA support is currently disabled. Please enter position manually.")) 
  (let* ((user-id (http-authenticated-user handler request))
         (race-id (get-routing-request-race-id request))
         (host |host|)
         (port |port|))
    (log2:info "User:~a RaceID:~a Port:~a" user-id race-id port)
    (unless (ignore-errors
             (numberp (parse-integer port)))
      (error "Invalid NMEA port ~a" port))
    (unless (nmea-connection user-id race-id)
      (reset-nmea-listener user-id race-id host port))
    (with-output-to-string (s)
      (json s
            (get-nmea-position user-id race-id)))))

(defun |resetNMEAConnection| (handler request response &key (|host| "nmea.realsail.net") (|port| ""))
  (unless *disable-nmea*
    (error "NMEA support is currently disabled. Please enter position manually."))
  (let* ((user-id (http-authenticated-user handler request))
         (race-id (get-routing-request-race-id request))
         (host |host|)
         (port |port|))
    (log2:info "User:~a RaceID:~a Connection: ~a:~a" user-id race-id host port)
    (cond ((equal port "")
           (remove-nmea-listener user-id race-id)
           (format nil "Disconnected from ~a:~a" host port))
          (t
           (unless (ignore-errors
                    (numberp (parse-integer port)))
             (error "Invalid NMEA port ~a" port))
           (reset-nmea-listener user-id race-id host port)
           (format nil "Connected to ~a:~a" host port))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

(defun load-html-file (path response &key substitutions)
  ;; The FILE-HANDLER response handler does a lot more - 
  ;; Think about redesigning this
  (with-open-file (f path :element-type 'character :external-format :utf-8)
    (let ((buffer (make-array (file-length f) :element-type 'character)))
      (read-sequence buffer f)
      (dolist (subst substitutions)
        (destructuring-bind (marker . value)
            subst
          (let ((start (search marker buffer)))
            (when (numberp start)
              (setf buffer
                    (concatenate 'string
                                 (subseq buffer 0 start)
                                 value
                                 (subseq buffer (+ start (length marker)))))))))
      (setf (http-body response) buffer)))
  (setf (http-header response :|Content-Type|)
        (get-mime-for-extension (pathname-type path))))

(defun load-file (path response)
  ;; The FILE-HANDLER response handler does a lot more - 
  ;; Think about redesigning this
  (with-open-file (f path :element-type '(unsigned-byte 8) :external-format :utf-8)
    (let ((buffer (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence buffer f)
      (setf (http-body response) buffer)))
  (setf (http-header response :|Content-Type|)
        (get-mime-for-extension (pathname-type path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined races

(defun create-uuid ()
  (with-open-file (f "/proc/sys/kernel/random/uuid")
    (read-line f)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
