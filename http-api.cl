;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2022-05-07 00:55:50>


(in-package :virtualhelm)

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
                                    (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                                                      '("web"))))))
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
                                (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                                                  '("web"))))))
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
               (etypecase race-info
                 (race-info-rs "router-rs")
                 (race-info-vr "router-vr")))
             (path (merge-pathnames (make-pathname :name page-base-name :type "html")
                                    (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                                                      '("web")))))
             (query (parameters request)))
        (log2:info "race-id: ~a" race-id)
        (log2:info "type: ~a" (type-of (race-info race-id)))
        (setf (http-header response :|Content-Location|)
              (path request))
        (load-html-file path response :substitutions (list (cons "GOOGLE_API_KEY" *api-key*))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

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
         (sql:?delete 'virtualhelm.user_prov
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

(defun |getRouteRS| (handler request response &key
                                                (|polarsID| "aA32bToWbF")
                                                |latStart|
                                                |lonStart|
                                                |latDest|
                                                |lonDest|
                                                (|cycleTS| (available-cycle (now)) cycle-supplied-p)
                                                (|duration| (* *rs-max-hours* 60 60))
                                                (|resolution| "1p00"))
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (user-id
               (http-authenticated-user handler request))
             (cycle (if cycle-supplied-p
                        (make-cycle :timestamp (parse-datetime |cycleTS|))
                        |cycleTS|))
             (routing
               (make-routing :interpolation :bilinear
                             :resolution |resolution|
                             :polars |polarsID|
                             :options '("realsail")
                             :penalties (make-penalty :sail 0.975d0 :tack 1d0 :gybe 1d0)
                             :stepmax (min (* *rs-max-hours* 60 60) ;; 2d
                                           (read-arg |duration|))
                             :cycle cycle
                             :merge-start 6d0
                             :merge-window 0d0
                             :start (make-latlng :lat (coerce (read-arg |latStart|) 'double-float)
                                                 :lng (coerce (read-arg |lonStart|) 'double-float))
                             :dest  (make-latlng :lat (coerce (read-arg |latDest|) 'double-float)
                                                 :lng (coerce (read-arg |lonDest|) 'double-float))))
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

(defun |getRouteVR| (handler request response &key
                                                (|polarsID| "1")
                                                |latStart|
                                                |lonStart|
                                                |latDest|
                                                |lonDest|
                                                (|cycleTS| (available-cycle (now)) cycle-supplied-p)
                                                (|options|  '("hull" "foil" "winch" "heavy" "light" "reach"))
                                                (|duration| "86400"))
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (user-id
               (http-authenticated-user handler request))
             (cycle (if cycle-supplied-p
                        (make-cycle :timestamp (parse-datetime |cycleTS|))
                        |cycleTS|))
             (routing
               (make-routing :interpolation :vr
                             :resolution "1p00"
                             :polars |polarsID|
                             :options |options|
                             :penalties  (make-penalty :sail 0.9375d0 :tack 0.9375d0 :gybe 0.9375d0)
                             :stepmax (min (* *vr-max-hours* 60 60)
                                           (read-arg |duration|))
                             :cycle cycle
                             :merge-start 4d0
                             :merge-window 1d0
                             :minwind t
                             :start (make-latlng :lat (coerce (read-arg |latStart|) 'double-float)
                                                 :lng (coerce (read-arg |lonStart|) 'double-float))
                             :dest  (make-latlng :lat (coerce (read-arg  |latDest|) 'double-float)
                                                 :lng (coerce (read-arg |lonDest|) 'double-float))))
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
(defun |getWind| (handler request response &key (|time|) (|basetime|) (|offset|) (|resolution|) |north| |east| |west| |south| (|ddx| "0.5") (|ddy| "0.5") (|ySteps|) (|xSteps|))
  (declare (ignore |ySteps| |xSteps|))
  ;; (sqlite-client:with-current-connection (c *db*)

    (log2:info "Basetime:~a Offset:~a Time:~a Resolution:~a N:~a S:~a W:~a E:~a" |basetime| |offset| |time| |resolution| |north| |south| |west| |east|)
    (assert (and |basetime| (or |time| |offset|)))
    (handler-case
        (let* ((*read-default-float-format*
                 'double-float)
               (base-time
                 (local-time:parse-rfc3339-timestring |basetime|))
               (resolution |resolution|)
               (requested-time
                 (if |time|
                     (local-time:parse-rfc3339-timestring |time|)
                     (local-time:adjust-timestamp  (local-time:parse-rfc3339-timestring |basetime|) (:offset :hour (read-arg |offset|)))))
               (user-id (http-authenticated-user handler request))
               (race-id (get-routing-request-race-id request))
               (cycle (make-cycle :timestamp base-time))
               (cycle-start-time base-time)
               (iparams (interpolation-parameters requested-time
                                                  :method (routing-interpolation routing)
                                                  :merge-start (routing-merge-start routing)
                                                  :merge-window (routing-merge-window routing)
                                                  :cycle cycle
                                                  :resolution resolution))
               (ddx (read-arg |ddx|))
               (ddy (read-arg |ddy|))
               (north (read-arg |north|))
               (south (read-arg |south|))
               (east (read-arg |east|))
               (west (read-arg |west|)))
          (log2:info "Requested time: ~a Using cycle: ~a" requested-time cycle)
          (when (< west 0d0) (incf west 360d0))
          (when (< east 0d0) (incf east 360d0))
          (when (< east west) (incf east 360d0))
            
          (assert (and (plusp ddx)
                       (plusp ddy)
                       (< south north)))
          (let ((wind-data
                  (loop
                    :for lat :from north :downto south :by ddy
                    :collect (loop
                               :for lon :from west :to east :by ddx
                               :collect (multiple-value-bind (dir speed)
                                            (let ((nlon
                                                    (if (> lon 0) (- lon 360) lon)))
                                              (interpolated-prediction lat nlon iparams))
                                          (list (round-to-digits dir 2)
                                                (round-to-digits speed 2)))))))
            (let ((body
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
              (values body))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e)))
      #+ccl(ccl::invalid-memory-access (e)
             (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
             (setf (status-code response) 500)
             (setf (status-text response) (format nil "~a" e))))
  ;;)
  )

(defun |getTWAPath| (handler request response &key |basetime| |time| |latA| |lngA| |lat| |lng|)
  ;; (sqlite-client:with-current-connection (c *db*)
  (handler-case
        (let* ((*read-default-float-format* 'double-float)
               (user-id
                 (http-authenticated-user handler request))
               (race-id (get-routing-request-race-id request))
               (base-time |basetime|)
               (time (parse-datetime |time|))
               (lat-a (read-arg |latA|))
               (lng-a (read-arg |lngA|))
               (lat (read-arg |lat|))
               (lng (read-arg |lng|)))
          (values
           (with-output-to-string (s)
             (json s (get-twa-path routing :base-time base-time :time time :lat-a lat-a :lng-a lng-a :lat lat :lng lng)))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e))))
  ;;)
  )

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
  ;; (sqlite-client:with-current-connection (c *db*)

    (let* ((user-id
             (http-authenticated-user handler request))
           (race-id
             (get-routing-request-race-id request))
           (race-info
             (race-info race-id)))
      (values
       (with-output-to-string (s)
         (json s race-info))))
  ;;)
  )

(defstruct raceinfo type name id gfs025 class start-time closing-time start-pos finish-pos closed)

(defun |getRaceList| (handler request response)
  ;; unauthenticated!
  (declare (ignore handler request response))
  (load-race-definitions :directory *races-dir*)
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
        :class (joref boat "label")
        :start-time (joref start "date")
        :start-pos  (make-latlng
                     :lat (joref start "lat")
                     :lng (joref start "lon")))))))

;;; The web page can't fetch the position from the Telnet port itself.
(defun |getBoatPosition| (handler request response &key (|host| "nmea.realsail.net") (|port| ""))
  (let* ((user-id (http-authenticated-user handler request))
         (race-id (get-routing-request-race-id request))
         (host |host|)
         (port |port|))
    (log2:info "User:~a RaceID:~a Port:~a" user-id race-id port)
    (unless (ignore-errors
             (numberp (parse-integer port)))
      (error "Invalid NMEA port ~a" port))
    (unless (nmea-connection user-id race-id)
      (add-nmea-listener user-id race-id host port))
    (with-output-to-string (s)
      (json s
            (get-nmea-position user-id race-id host port)))))

(defun |resetNMEAConnection| (handler request response &key (|host| "nmea.realsail.net") (|port| ""))
  ;; (sqlite-client:with-current-connection (c *db*)
    (let* ((user-id (http-authenticated-user handler request))
           (race-id (get-routing-request-race-id request))
           (host |host|)
           (port |port|))
      (log2:info "User:~a RaceID:~a Connection: ~a:~a" user-id race-id host port)
      (unless (nmea-connection user-id race-id)
        (setf (nmea-connection user-id race-id)
              (make-nmea-connection))) 
      (cond ((equal port "")
             (stop-nmea-listener user-id race-id host port)
             (format nil "Disconnected from ~a:~a" host port))
            (t
             (unless (ignore-errors
                      (numberp (parse-integer port)))
               (error "Invalid NMEA port ~a" port))
             (reset-nmea-listener user-id routing host port)
             (format nil "Connected to ~a:~a" host port))))
  ;;)
  )
           
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

