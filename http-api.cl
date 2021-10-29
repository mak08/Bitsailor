;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2021-10-29 21:25:03>


(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP API

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic handlers

(defun get-page (server handler request response)
  (sqlite-client:with-current-connection (c *db*)
    (handler-case 
        (let* ((user-id (http-authenticated-user handler request))
               (session (find-or-create-session user-id request response))
               (app (get-request-app request))
               (race-id (get-routing-request-race-id request))
               (routing (session-routing session race-id))
               (path (merge-pathnames (make-pathname :name app :type "html")
                                      (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                                                        '("web"))))))
          (log2:info "race-id: ~a" race-id)
          (set-routing-parameters session routing (parameters request))
          (setf (http-header response :|Content-Location|)
                (path request))
          (load-file path response))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e))))))

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
  (sqlite-client:with-current-connection (c *db*)
    (handler-case 
        (let* ((user-id (http-authenticated-user handler request))
               (session (find-or-create-session user-id request response))
               (app (get-request-app request))
               (race-id (get-routing-request-race-id request))
               (routing (session-routing session race-id))
               (path (merge-pathnames (make-pathname :name "router" :type "html")
                                      (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                                                        '("web")))))
               (query (parameters request)))
          (log2:info "race-id: ~a" race-id)
          (set-routing-parameters session routing (parameters request))
          (setf (http-header response :|Content-Location|)
                (path request))
          (load-html-file path response :substitutions (list (cons "GOOGLE_API_KEY" *api-key*))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e))))))

;;; This function is called from a dynamic handler
(defun activate-account (server handler request response)
  (sqlite-client:with-current-connection (c *db*)
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
  (sqlite-client:with-current-connection (c *db*)
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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setRoute
(defun |setRoute| (handler request response &key |pointType| |lat| |lng|)
  (sqlite-client:with-current-connection (c *db*)
    (handler-case
        (let* ((user-id (http-authenticated-user handler request))
               (session (find-or-create-session user-id request response))
               (race-id (get-routing-request-race-id request))
               (routing (session-routing session race-id))
               (lat (coerce (read-arg |lat|) 'double-float))
               (lng (coerce (read-arg |lng|) 'double-float))
               (position (make-latlng :latr% (rad lat) :lngr% (rad lng))))
          (log2:info "~a: Position=~a" |pointType| position)
          (log2:trace "Session: ~a, Request: ~a" session request)
          (when (point-on-land-p (cl-geomath:make-latlng :lat lat :lng lng))
            (log2:warning "~a ~a: ~a is on land" user-id race-id position))
          (set-routing-parameter session routing |pointType| position)
          (values
           (with-output-to-string (s)
             (json s routing))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setParameter

(defun |setParameter| (handler request response &key |name| (|value| nil))
  (sqlite-client:with-current-connection (c *db*)
    
    (let* ((user-id (http-authenticated-user handler request))
           (session (find-or-create-session user-id request response))
           (race-id (get-routing-request-race-id request))
           (routing (session-routing session race-id)))
      (set-routing-parameter session routing |name| |value|) 
      (setf (http-header response :|Content-Location|)
            (path request))
      (values "true"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getRoute

(defun |getRouteRS| (handler request response &key
                                                (|polarsID| "aA32bToWbF")
                                                |latStart|
                                                |lonStart|
                                                |latDest|
                                                |lonDest|
                                                (|duration| "86400"))
  (sqlite-client:with-current-connection (c *db*)
    (handler-case
        (let* ((*read-default-float-format* 'double-float)
               (user-id
                 (http-authenticated-user handler request))
               (lat-start (coerce (read-arg |latStart|) 'double-float))
               (lon-start (coerce (read-arg |lonStart|) 'double-float))
               (lat-dest (coerce (read-arg  |latDest|) 'double-float))
               (lon-dest (coerce (read-arg |lonDest|) 'double-float))
               (duration (min (* *rs-max-hours* 60 60)  ;; 2d
                              (read-arg |duration|)))
               (routing
                 (make-routing :interpolation :enorm
                               :polars |polarsID|
                               :options '("realsail")
                               :stepmax duration
                               :merge-start 6d0
                               :merge-window 0d0
                               :start (make-latlng :lat lat-start :lng lon-start)
                               :dest  (make-latlng :lat lat-dest :lng lon-dest)))
               (routeinfo
                 (get-route routing)))
          (log2:info "User:~a Race:-- Status ~a ~a"
                     user-id
                     (routeinfo-status routeinfo)
                     (routeinfo-stats routeinfo))
          (values
           (with-output-to-string (s)
             (json s routeinfo))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e))))))

(defun |getRoute| (handler request response)
  (sqlite-client:with-current-connection (c *db*)

    (let ((race-id (get-routing-request-race-id request)))
      (handler-case
          (let* ((user-id
                   (http-authenticated-user handler request))
                 (session
                   (find-or-create-session user-id request response))
                 (routing
                   (session-routing session race-id))
                 (routeinfo
                   (get-route routing)))
          (log2:info "User:~a Race:~a Status ~a ~a"
                     user-id
                     race-id
                     (routeinfo-status routeinfo)
                     (routeinfo-stats routeinfo))
            (values
             (with-output-to-string (s)
               (json s routeinfo))))
        (error (e)
          (log2:error "~a" e)
          (setf (status-code response) 500)
          (setf (status-text response) (format nil "~a" e)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getSession

;;; Call router with
;;; - user id (via authentication)
;;; - race id (query param)
;;; - boat name (query param)
;;; - start pos
;;; - boat options

;;; Session Data
;;; * The following data is stored and returned to the client:
;;; - forecast duration (or hard-code 384h?)
;;; - manual gates
;;; - race definition
;;;   - race id
;;;   - dest pos
;;;   - gates
;;;   - polars id

;;; JSON cannot print hashtables but SESSION now contains one.
;;; For now we just return the requested routing.
(defun |getSession| (handler request response)
  (sqlite-client:with-current-connection (c *db*)

    (let* ((user-id
             (http-authenticated-user handler request))
           (session
             (find-or-create-session user-id request response))
           (race-id
             (get-routing-request-race-id request))
           (routing
             (session-routing session race-id))
           (leg-data
             (race-info race-id)))
      (values
       (with-output-to-string (s)
         (json s routing))))))


(defun |removeSession| (handler request response)
  (sqlite-client:with-current-connection (c *db*)
    (let* ((user-id
             (http-authenticated-user handler request))
           (session
             (find-or-create-session user-id request response)))
      (remove-session session)
      (values
       (with-output-to-string (s)
         (json s t))))))

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
(defun |getWind| (handler request response &key (|time|) (|basetime|) (|offset|) |north| |east| |west| |south| (|ddx| "0.5") (|ddy| "0.5") (|ySteps|) (|xSteps|))
  (declare (ignore |ySteps| |xSteps|))
  (sqlite-client:with-current-connection (c *db*)

    (log2:info "Basetime:~a Offset:~a Time:~a N:~a S:~a W:~a E:~a" |basetime| |offset| |time| |north| |south| |west| |east|)
    (assert (and |basetime| (or |time| |offset|)))
    (handler-case
        (let* ((*read-default-float-format*
                 'double-float)
               (base-time
                 (local-time:parse-rfc3339-timestring |basetime|))
               (requested-time
                 (if |time|
                     (local-time:parse-rfc3339-timestring |time|)
                     (local-time:adjust-timestamp  (local-time:parse-rfc3339-timestring |basetime|) (:offset :hour (read-arg |offset|))))))
          (log2:info "Requested time: ~a" requested-time)
          (let* ((user-id (http-authenticated-user handler request))
                 (session (find-or-create-session user-id request response))
                 (race-id (get-routing-request-race-id request))
                 (routing (session-routing session race-id))
                 (cycle (make-cycle :timestamp base-time))
                 (cycle-start-time base-time)
                 (iparams (interpolation-parameters requested-time
                                                    :method (routing-interpolation routing)
                                                    :merge-start (routing-merge-start routing)
                                                    :merge-window (routing-merge-window routing)
                                                    :cycle cycle))
                 (ddx (read-arg |ddx|))
                 (ddy (read-arg |ddy|))
                 (north (read-arg |north|))
                 (south (read-arg |south|))
                 (east (read-arg |east|))
                 (west (read-arg |west|)))
            (log2:info "Using cycle: ~a" cycle)
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
              (let ((encoding (http-header request :accept-encoding))
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
                (values body)))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e)))
      #+ccl(ccl::invalid-memory-access (e)
             (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
             (setf (status-code response) 500)
             (setf (status-text response) (format nil "~a" e))))))

(defun |getTWAPath| (handler request response &key |basetime| |time| |latA| |lngA| |lat| |lng|)
  (sqlite-client:with-current-connection (c *db*)
    (handler-case
        (let* ((*read-default-float-format* 'double-float)
               (user-id
                 (http-authenticated-user handler request))
               (session (find-or-create-session user-id request response))
               (race-id (get-routing-request-race-id request))
               (routing (session-routing session race-id))
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
        (setf (status-text response) (format nil "~a" e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getWindForecast
;;;
;;; Return current and old forecast

(defstruct forecast current previous interpolated)
(defstruct windinfo date cycle dir speed) 

(defun |getWindForecast| (handler request response &key |time| |lat| |lng|)
  (declare (ignore handler))
  (sqlite-client:with-current-connection (c *db*)
    (setf (http-header response :|Access-Control-Allow-Origin|) (http-header request :|origin|))
    (setf (http-header response :|Access-Control-Allow-Credentials|) "true")
    (handler-case
        (let ((*read-default-float-format* 'double-float))
          (let* (
                 (user-id
                   (http-authenticated-user handler request))
                 (session
                   (find-or-create-session user-id request response))
                 (race-id
                   (get-routing-request-race-id request))
                 (routing
                   (session-routing session race-id))
                 (forecast-time
                   (timestamp-truncate (parse-rfc3339-timestring |time|) 300))
                 (lat
                   (read-arg |lat|))
                 (lng
                   (read-arg |lng|))
                 (cycle1
                   (available-cycle forecast-time))
                 (cycle0
                   (previous-cycle cycle1))) 
            (multiple-value-bind (dir1 speed1)
                (vr-prediction lat lng :timestamp forecast-time :cycle cycle1)
              (multiple-value-bind (dir0 speed0)
                  (vr-prediction lat lng :timestamp forecast-time :cycle cycle0)
                (multiple-value-bind (dir speed)
                    (interpolated-prediction lat lng (interpolation-parameters forecast-time
                                                                               :method (routing-interpolation routing)
                                                                               :merge-start (routing-merge-start routing)
                                                                               :merge-window (routing-merge-window routing)
                                                                               ))
                  (values
                   (with-output-to-string (s)
                     (json s (make-forecast
                              :previous (make-windinfo :cycle cycle0
                                                       :dir (round-to-digits dir0 2)
                                                       :speed (round-to-digits (m/s-to-knots speed0) 2))
                              :current (make-windinfo :cycle cycle1
                                                      :dir (round-to-digits dir1 2)
                                                      :speed (round-to-digits (m/s-to-knots speed1) 2))
                              :interpolated (make-windinfo :cycle cycle1
                                                           :dir (round-to-digits dir 2)
                                                           :speed (round-to-digits (m/s-to-knots speed) 2)))))))))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e)))
      #+ccl(ccl::invalid-memory-access (e)
             (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
             (setf (status-code response) 500)
             (setf (status-text response) (format nil "~a" e))))))

(defun timestamp-truncate (timestamp seconds)
  (universal-to-timestamp (* seconds (truncate (timestamp-to-universal timestamp) seconds))))

                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Probe Wind

(defun |probeWind| (handler request response &key |time| |lat| |lng| |cycle|)
  (sqlite-client:with-current-connection (c *db*)
    (setf (http-header response :|Access-Control-Allow-Origin|) (http-header request :|origin|))
    (setf (http-header response :|Access-Control-Allow-Credentials|) "true")
    (handler-case
        (let ((*read-default-float-format* 'double-float)
              (race-id (get-routing-request-race-id request)))
          (let* ((user-id
                   (http-authenticated-user handler request))
                 (session
                   (find-or-create-session user-id request response))
                 (routing
                   (session-routing session race-id))
                 (forecast-time
                   (parse-rfc3339-timestring |time|))
                 (lat (read-arg |lat|))
                 (lng (read-arg |lng|))
                 (default-cycle
                   (available-cycle (now)))
                 (cycle
                   (or (and |cycle|
                            (make-cycle :timestamp (parse-timestring |cycle|)))
                       default-cycle)))
            (multiple-value-bind (dir speed)
                (interpolated-prediction lat lng (make-iparams :previous (prediction-parameters forecast-time :cycle cycle)
                                                               :current (prediction-parameters forecast-time :cycle cycle)))
              (values
               (with-output-to-string (s)
                 (json s
                       (list (round-to-digits dir 2)
                             (round-to-digits (m/s-to-knots speed) 2))))))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e))))))

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
  (sqlite-client:with-current-connection (c *db*)

    (let* ((user-id
             (http-authenticated-user handler request))
           (session
             (find-or-create-session user-id request response))
           (race-id
             (get-routing-request-race-id request))
           (race-info
             (race-info race-id)))
      (values
       (with-output-to-string (s)
         (json s race-info))))))

(defstruct raceinfo type name id class start-time closing-time start-pos finish-pos closed)

(defun |getRaceList| (handler request response)
  ;; unauthenticated!
  (declare (ignore handler request response))
  (let ((filename  (merge-pathnames *races-dir* (make-pathname :name :wild :type "json"))))
    (log2:info "Loading races from ~a" filename)
    (load-race-definitions :directory filename)
    (let ((races (list)))
      (map-races 
       (lambda (k v)
         (declare (ignore k))
         (push (get-raceinfo (race-info-data v)) races)))
      (with-output-to-string (s)
        (json s races)))))

(defun get-raceinfo (race)
  (cond
    ((joref race "objectId")
     (make-raceinfo
      :type "rs"
      :name (joref race "name")
      :id (joref race "objectId")
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
  (sqlite-client:with-current-connection (c *db*)

    (let* ((user-id (http-authenticated-user handler request))
           (race-id (get-routing-request-race-id request))
           (session (find-or-create-session user-id request response))
           (routing (session-routing session race-id))
           (host |host|)
           (port |port|))
      (log2:info "User:~a RaceID:~a Port:~a" user-id race-id port)
      (unless (ignore-errors
               (numberp (parse-integer port)))
        (error "Invalid NMEA port ~a" port))
      (unless (routing-nmea-connection routing)
        (setf (routing-nmea-connection routing)
              (make-nmea-connection))
        (reset-nmea-listener user-id routing host port))
      (with-output-to-string (s)
        (json s
              (get-nmea-position (routing-nmea-connection routing) host port))))))

(defun |resetNMEAConnection| (handler request response &key (|host| "nmea.realsail.net") (|port| ""))
  (sqlite-client:with-current-connection (c *db*)
    (let* ((user-id (http-authenticated-user handler request))
           (race-id (get-routing-request-race-id request))
           (session (find-or-create-session user-id request response))
           (routing (session-routing session race-id))
           (host |host|)
           (port |port|))
      (log2:info "User:~a RaceID:~a Connection: ~a:~a" user-id race-id host port)
      (unless (routing-nmea-connection routing)
        (setf (routing-nmea-connection routing)
              (make-nmea-connection))) 
      (cond ((equal port "")
             (stop-nmea-listener routing host port)
             (format nil "Disconnected from ~a:~a" host port))
            (t
             (unless (ignore-errors
                      (numberp (parse-integer port)))
               (error "Invalid NMEA port ~a" port))
             (reset-nmea-listener user-id routing host port)
             (format nil "Connected to ~a:~a" host port))))))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

;;; Keep session table when reloading system!
(defvar *session-ht* (make-hash-table :test #'equalp))

(defvar +session-ht-lock+
  (bordeaux-threads:make-lock "session-ht"))

(defun update-session-ht (key value)
  (bordeaux-threads:with-lock-held (+session-ht-lock+)
    (setf (gethash key *session-ht*) value))) 

(defun create-session (&key (session-id (make-session-id)) (user-id) (race-id "default"))
  (let ((session (make-session :session-id session-id :user-id user-id)))
    (setf (gethash race-id (session-routings session))
          (create-routing :race-id race-id))
    (values session)))

(defun session-routing (session race-id)
  (or
   (gethash race-id (session-routings session))
   (log2:trace "Creating routing for session ~a race ~a" (session-session-id session) race-id)
   (setf (gethash race-id (session-routings session))
         (create-routing :race-id race-id))))

(defun find-or-create-session (user-id request response)
  (let* ((session-cookie
          (get-cookie request "SessionID"))
         (race-id (get-routing-request-race-id request))
         (session-id)
         (session))
    (cond (session-cookie
           (setf session-id (cookie-value session-cookie))
           (let ((stored-session (gethash session-id *session-ht*)))
             (cond
               ((null stored-session)
                (log2:info "Session ~a unknown, creating." session-id)
                (setf session
                      (update-session-ht session-id 
                                         (create-session :session-id session-id
                                                         :user-id user-id
                                                         :race-id race-id))))
               (t
                (log2:trace "Session ~a found." session-id)
                (setf session stored-session)))))
          (t
           (setf session-id (make-session-id))
           (set-cookie response "SessionID" session-id :options '())
           (setf session
                 (update-session-ht session-id
                                    (create-session :session-id session-id
                                                    :race-id race-id)))
           (log2:info "New session ~a created." session-id)))
    session))

(defun remove-session (session)
  (log2:info "Removing session ~a" session))

(defun load-html-file (path response &key substitutions)
  ;; The FILE-HANDLER response handler does a lot more - 
  ;; Think about redesigning this
  (with-open-file (f path :element-type 'character)
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
  (with-open-file (f path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence buffer f)
      (setf (http-body response) buffer)))
  (setf (http-header response :|Content-Type|)
        (get-mime-for-extension (pathname-type path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined races


(defun set-routing-parameters (session routing pairs)
  (map nil
       (lambda (pair)
         (destructuring-bind (name value)
             pair
           (set-routing-parameter session routing name value)))
       (map-routing-parameters pairs)))

(defun map-routing-parameters (pairs)
  (let ((result-pairs (list))
        (*read-default-float-format* 'double-float))
    (dolist (pair pairs (nreverse result-pairs))
      (destructuring-bind (name value)
          pair
        (cond
          ((string= name "app")
           )
          ((string= name "startlat")
           (let ((startlon (cadr (assoc "startlon" pairs :test #'string=))))
             (unless startlon
               (error "Missing startlon"))
             (push (list "start"
                         (make-latlng :lat (read-arg value)
                                      :lng (read-arg startlon)))
                   result-pairs)))
          ((string= name "startlon")
           (unless (cadr (assoc "startlat" pairs :test #'string=))
             (error "Missing startlat")))
          (t
           (push pair result-pairs)))))))

(defun set-routing-parameter (session routing name value)
  (log2:info "Session ~a: ~a=~a" (session-session-id session) name value)
  (cond
    ((string= name "race")
     )
    ((or (string= name "starttime")
         (string= name "ts"))
     (setf (routing-starttime routing)
           (when (and value
                      (> (length value) 0))
             value)))
    ((or (string= name "cycle"))
     (cond
       ((null value)
        (setf (routing-cycle routing)
              nil))
       (t
        (assert (eql (length value)
                     (length "2020-10-24T00:00:00Z")))
        (setf (routing-cycle routing) (make-cycle :timestamp (parse-timestring value))))))
    ((string= name "polars")
     (setf (routing-polars routing)  value))
    ((string= name "options")
     (setf (routing-options routing) (cl-utilities:split-sequence #\, value)))
    ((string= name "minwind")
     (setf (routing-minwind routing) (string= value "true")))
    ((string= name "duration")
     (let ((duration-hrs
            (read-arg value)))
       (setf (routing-stepmax routing)
             (* duration-hrs 3600))))
    ((string= name "searchangle")
     (let ((fan (read-arg value)))
       (setf (routing-fan routing) fan)))
    ((string= name "slat")
     (unless (routing-start routing)
       (setf (routing-start routing)
             (make-latlng)))
     (setf (routing-start routing)
           (make-latlng :lat (parse-float value :type 'double-float)
                        :lng (latlng-lng (routing-start routing)))))
    ((string= name "slon")
     (unless (routing-start routing)
       (setf (routing-start routing)
             (make-latlng)))
     (setf (routing-start routing)
           (make-latlng :lat (latlng-lat (routing-start routing))
                        :lng (parse-float value :type 'double-float))))
    ((string= name "start")
     (setf (routing-start routing)
           (etypecase value
             (latlng value)
             (string (find-place value)))))
    ((string= name "dest")
     (setf (routing-dest routing)
           (etypecase value
             (latlng value)
             (string (find-place value)))))
    (t
     (log2:warning "Unhandled parameter ~a=~a" name value)))
    (values))

(defun make-session-id ()
  (create-uuid))

(defun create-uuid ()
  (with-open-file (f "/proc/sys/kernel/random/uuid")
    (read-line f)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

