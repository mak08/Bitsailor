;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2024-07-20 21:31:33>

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
  (cond
    ((string= presets "VR")
     (if (string=
          (joref (race-info-data (race-info race-id)) "fineWinds")
          "TRUE")
         (knots-to-m/s 1d0)
         (knots-to-m/s 2d0)))
    (t
     0d0)))

(defun get-routing-presets (presets
                            &key
                              (race-id)
                              (polars-id)
                              (starttime nil)
                              (resolution "1p00" resolution-provided-p)
                              (gfs-mode "06h")
                              (options)
                              (energy 100)
                              (tack)
                              (sail nil)
                              (stepmax (* 24 60 60))
                              (cycle (current-cycle))
                              (slat)
                              (slon)
                              (dlat)
                              (dlon))
  (let* ((race-info (race-info race-id))
         (vr-finewinds
           (and (string= presets "VR")
                (string= "TRUE"
                         (joref (race-info-data race-info) "fineWinds"))))
         (polars-id (format nil "~a" (or polars-id
                                         (joref (joref (race-info-data race-info) "boat") "polar_id"))))
         (options
           (or options
               '("hull" "foil" "winch" "heavy" "light" "reach")))
         (polars (get-combined-polars polars-id (encode-options options))))
    (when (and vr-finewinds
               (not resolution-provided-p))
      (setf resolution "0p25"))
    (make-routing
     :race-info   (race-info race-id)
     :fan *max-angle*
     :start (when (and slat slon) (make-latlng :lat slat :lng slon))
     :dest  (when (and dlat dlon) (make-latlng :lat dlat :lng dlon))
     :starttime starttime
     :stepmax (min (* *max-route-hours* 3600) stepmax)
     :options options
     :energy energy
     :tack (cond
             ((string= tack "port")
              90d0)
             ((string= tack "stbd")
              -90d0)
            (t
             (log2:warning "No valid initial tack ~a" tack)
             nil))
     :sail sail
     :resolution resolution
     :minwind (determine-minwind presets race-id)
     :gfs-mode gfs-mode
     :grib-source (if (and (string= resolution "0p25")
                           vr-finewinds)
                      :vr
                      :noaa)
     :interpolation (cond
                      (vr-finewinds
                       :enorm)
                      (t
                       :vr))
     :polars polars
     :cycle cycle
     :merge-start (if vr-finewinds
                      6d0
                      4d0)
     :merge-window (if vr-finewinds 3d0 2d0)
     :winch-mode (if (member "winch" options :test #'string=)
                     "pro"
                     "std")
     :penalties  (make-penalty :sail 0.9375d0 :tack 0.9375d0 :gybe 0.85d0)
     :simplify-route t)))

(defun |getRoute| (handler request response &key
                                              (|presets| "VR")
                                              (|raceId| nil)
                                              (|options| nil)
                                              (|energy| "100")
                                              (|tack| nil)
                                              (|sail| nil)
                                              (|resolution| "1p00")
                                              (|gfsMode| "06h")
                                              |polarsId|
                                              |slat|
                                              |slon|
                                              |dlat|
                                              |dlon|
                                              (|startTime| nil)
                                              (|cycleTS| nil cycle-supplied-p)
                                              (|duration| (* *max-route-hours* 3600)
                                                          duration-supplied-p))
  (handler-bind
      ((error (lambda (e)
                    (log2:error "~a" e)
                    (sb-debug:print-backtrace)
                    (setf (status-code response) 500)
                    (setf (status-text response) (format nil "~a" e))
                    (return-from |getRoute| (format nil "~a" e)))))
    (let* ((*read-default-float-format* 'double-float)
             (user-id
               (http-authenticated-user handler request))
             (cycle (if cycle-supplied-p
                        (make-cycle :timestamp (parse-datetime |cycleTS|))
                        (available-cycle (now))))
             (routing
               (get-routing-presets |presets|
                                    :race-id |raceId|
                                    :polars-id |polarsId|
                                    :gfs-mode |gfsMode|
                                    :options (cl-utilities:split-sequence #\, |options|)
                                    :energy (read-arg |energy| 'double-float)
                                    :tack |tack|
                                    :sail |sail|
                                    :resolution |resolution|
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
           (json s routeinfo))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TWA Path

(defun |getTWAPath| (handler request response &key |presets| |raceId| |options| |gfsMode| |polarsId| |cycle| |time| |resolution| |latA| |lngA| |lat| |lng|)
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (user-id
               (http-authenticated-user handler request))
             (race-id (get-routing-request-race-id request))
             (cycle (when |cycle| (make-cycle :timestamp (parse-timestring |cycle|))))
             (routing
               (get-routing-presets |presets|
                                    :race-id |raceId|
                                    :polars-id |polarsId|
                                    :gfs-mode |gfsMode|
                                    :options (cl-utilities:split-sequence #\, |options|)
                                    :resolution |resolution|
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
;;; Commands

(defun handle-options (server handler request response)
  (format t "~a~%" request)
  (format t "~a~%" (http-body request))
  (setf (http-header response :|Access-Control-Allow-Origin|)
        "*")
  (setf (http-header response :|Access-Control-Allow-Methods|)
        "POST, GET, OPTIONS")
  (setf (http-header response :|Access-Control-Allow-Headers|)
        "Authorization, Content-Type")
  (setf (status-code response) 204)
  (setf (status-text response) "No Content"))

(defun execute-commands (server handler request response)
  (format t "~a~%" request)
  (format t "~a~%" (http-body request))
  (setf (http-header response :|Content-Type|)
        "application/json")
  
  (let* ((payload (parse-json (http-body request)))
         (device-id (joref payload "deviceId"))
         (body (joref payload "body"))
         (url
           (format nil "https://prod.vro.sparks.virtualregatta.com/rs/device/~a/LogEventRequest" device-id)))
    (log2:info "~a ~a~%" url  body)
    (let ((reply
            (curl:http url :method :post :body (with-output-to-string (s) (json s body  :preserve-slotname-case t)))))
      (setf (http-body response) (curl:http-response-body reply)))))

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

(defstruct raceinfo type name id gfs025 record class level category start-time closing-time start-pos finish-pos closed)

(defun |getRaceListAdmin| (handler request response &key (|forceReload| "false"))
  ;; Admin only!
  (declare (ignore handler request response))
  (get-race-list :force-reload (string= |forceReload| "true")))

(defun |getRaceList| (handler request response)
  ;; unauthenticated!
  (declare (ignore handler request response))
  (get-race-list :force-reload nil))

(defun get-race-list (&key (force-reload nil))
  (let* ((races (list)))
    (when force-reload
      (load-race-definitions :directory *races-dir*))
    (map-races 
     (lambda (k v)
       (declare (ignore k))
       (push (get-raceinfo (race-info-data v)) races)))
    (with-output-to-string (s)
      (json s races)
      )))

(defun get-raceinfo (race)
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
     :level (joref race "priceLevel")
     :category  (joref race "vsrLevel")
     :start-time (joref start "date")
     :start-pos  (make-latlng
                  :lat (joref start "lat")
                  :lng (joref start "lon")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Router status & stats

(defstruct (routerstatus (:constructor routerstatus (requestcount datasource))) requestcount datasource)

(defun |getStatistics| (handler request response)
  (let ((datasource
          (cond
            ((string= cl-weather:*noaa-gfs-path* cl-weather::+NCEP-NOMADS+)
             "NOMADS")
            ((string= cl-weather:*noaa-gfs-path* cl-weather::+NCEP-FTPPRD+)
             "FTPPRD")
            (t
             cl-weather:*noaa-gfs-path*))))
    (with-output-to-string (s)
      (json s
            (routerstatus (length *last-request*)
                          datasource)))))

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
