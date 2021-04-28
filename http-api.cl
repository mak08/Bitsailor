;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2021-04-28 20:19:55>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP API

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-page
;;;
;;; Return the main page (this is currently the only HTML page)
;;; This function is called as a dynamic handler. It is NOT registered.
(defun get-page (server handler request response)
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
              (get-routing-url session race-id))
        (load-file path response))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setRoute
(defun |setRoute| (handler request response &key |pointType| |lat| |lng|)
  (handler-case
      (let* ((user-id (http-authenticated-user handler request))
             (session (find-or-create-session user-id request response))
             (race-id (get-routing-request-race-id request))
             (routing (session-routing session race-id))
             (lat (coerce (read-from-string |lat|) 'double-float))
             (lng (coerce (read-from-string |lng|) 'double-float))
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
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setParameter

(defun |setParameter| (handler request response &key |name| (|value| nil))
  (let* ((user-id (http-authenticated-user handler request))
         (session (find-or-create-session user-id request response))
         (race-id (get-routing-request-race-id request))
         (routing (session-routing session race-id)))
    (set-routing-parameter session routing |name| |value|) 
    (setf (http-header response :|Content-Location|)
          (get-routing-url session race-id))
    (values "true")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getRoute

(defun |getRoute| (handler request response)
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
          (values
           (with-output-to-string (s)
             (json s routeinfo))))
      (error (e)
        (log2:error "~a" e)
        (setf (status-code response) 500)
        (setf (status-text response) (format nil "~a" e))))))

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

;;; JSON cannot print hashtables but SESSIOn now contains one.
;;; For now we just return the requested routing.
(defun |getSession| (handler request response)
  (let* ((user-id
          (http-authenticated-user handler request))
         (session
          (find-or-create-session user-id request response))
         (race-id
          (get-routing-request-race-id request))
         (routing
          (session-routing session race-id))
         (leg-data
          (gethash race-id *races-ht*)))
    (values
     (with-output-to-string (s)
       (json s routing)))))

(defun |getLegInfo| (handler request response)
  (let* ((user-id
          (http-authenticated-user handler request))
         (session
          (find-or-create-session user-id request response))
         (race-id
          (get-routing-request-race-id request))
         (leg-info
           (get-leg-info race-id user-id)))
    (values
     (with-output-to-string (s)
       (json s leg-info)))))

(defun get-leg-info (race-id user-id)
  (gethash race-id *races-ht*))

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
  (declare (ignore request |ySteps| |xSteps|))
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
                  (local-time:adjust-timestamp  (local-time:parse-rfc3339-timestring |basetime|) (:offset :hour (read-from-string |offset|))))))
        (log2:info "Requested time: ~a" requested-time)
        (multiple-value-bind (date cycle)
            (timestamp-to-timespec base-time)
          (let* ((cycle-start-time (timespec-to-timestamp date cycle))
                 (user-id (http-authenticated-user handler request))
                 (session (find-or-create-session user-id request response))
                 (ddx (read-from-string |ddx|))
                 (ddy (read-from-string |ddy|))
                 (north (read-from-string |north|))
                 (south (read-from-string |south|))
                 (east (read-from-string |east|))
                 (west (read-from-string |west|)))
            (log2:info "Using cycle: ~a/~a ~a" date cycle cycle-start-time)
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
                                                 (cl-weather:vr-prediction lat nlon :date date :cycle cycle :timestamp requested-time))
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
                                                          :format '((:year 4) "-" (:month 2) "-" (:day 2) " Cycle " (:hour 2)) :timezone local-time:+utc-zone+)
                                :data wind-data))))))
               (values body))))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))
    #+ccl(ccl::invalid-memory-access (e)
           (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
           (setf (status-code response) 500)
           (setf (status-text response) (format nil "~a" e)))))

(defun |getTWAPath| (handler request response &key |basetime| |time| |latA| |lngA| |lat| |lng|)
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (user-id
              (http-authenticated-user handler request))
             (session (find-or-create-session user-id request response))
             (race-id (get-routing-request-race-id request))
             (routing (session-routing session race-id))
             (base-time |basetime|)
             (time (parse-datetime |time|))
             (lat-a (read-from-string |latA|))
             (lng-a (read-from-string |lngA|))
             (lat (read-from-string |lat|))
             (lng (read-from-string |lng|)))
        (values
         (with-output-to-string (s)
           (json s (get-twa-path routing :base-time base-time :time time :lat-a lat-a :lng-a lng-a :lat lat :lng lng)))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getWindForecast
;;;
;;; Return current and old forecast

(defstruct forecast current previous interpolated)
(defstruct windinfo date cycle dir speed) 

(defun |getWindForecast| (handler request response &key |time| |lat| |lng|)
  (declare (ignore handler))
  (setf (http-header response :|Access-Control-Allow-Origin|) (http-header request :|origin|))
  (setf (http-header response :|Access-Control-Allow-Credentials|) "true")
  (handler-case
      (let ((*read-default-float-format* 'double-float))
        (let* ((forecast-time
                (timestamp-truncate (parse-rfc3339-timestring |time|) 300))
               (lat (read-from-string |lat|))
               (lng (read-from-string |lng|)))
          (multiple-value-bind  (date1 cycle1)
              (available-cycle forecast-time)
            (multiple-value-bind (date0 cycle0)
                (previous-cycle date1 cycle1) 
              (multiple-value-bind (dir1 speed1)
                  (vr-prediction lat lng :timestamp forecast-time :date date1 :cycle cycle1)
                (multiple-value-bind (dir0 speed0)
                    (vr-prediction lat lng :timestamp forecast-time :date date0 :cycle cycle0)
                  (multiple-value-bind (dir speed)
                      (interpolated-prediction lat lng (interpolation-parameters forecast-time))
                    (values
                     (with-output-to-string (s)
                       (json s (make-forecast
                                :previous (make-windinfo :date date0
                                                         :cycle cycle0
                                                         :dir (round-to-digits dir0 2)
                                                         :speed (round-to-digits (m/s-to-knots speed0) 2))
                                :current (make-windinfo :date date1
                                                        :cycle cycle1
                                                        :dir (round-to-digits dir1 2)
                                                        :speed (round-to-digits (m/s-to-knots speed1) 2))
                                :interpolated (make-windinfo :date date1
                                                             :cycle cycle1
                                                             :dir (round-to-digits dir 2)
                                                             :speed (round-to-digits (m/s-to-knots speed) 2)))))))))))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))
    #+ccl(ccl::invalid-memory-access (e)
           (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
           (setf (status-code response) 500)
           (setf (status-text response) (format nil "~a" e)))))

(defun timestamp-truncate (timestamp seconds)
  (universal-to-timestamp (* seconds (truncate (timestamp-to-universal timestamp) seconds))))

                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Probe Wind

(defun |probeWind| (handler request response &key |time| |lat| |lng| |date| |cycle|)
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
               (lat (read-from-string |lat|))
               (lng (read-from-string |lng|)))
          (multiple-value-bind  (default-date default-cycle)
              (available-cycle (now))
            (multiple-value-bind (date cycle)
                (values (or |date| default-date)
                        (or (and |cycle| (read-from-string |cycle|))
                            default-cycle))
              (multiple-value-bind (dir speed)
                  (interpolated-prediction lat lng (make-iparams :previous (prediction-parameters forecast-time :date date :cycle cycle)
                                                                 :current (prediction-parameters forecast-time :date date :cycle cycle)))
                (values
                 (with-output-to-string (s)
                   (json s
                         (list (round-to-digits dir 2)
                               (round-to-digits (m/s-to-knots speed) 2))))))))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))
    #+ccl(ccl::invalid-memory-access (e)
           (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
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
;;; RealSail specific commads

(defstruct raceinfo name id class start-time closing-time start-pos finish-pos closed)

(defun |getRaceList| (handler request response)
  ;; unauthenticated!
  (declare (ignore handler request response))
  (let ((races (list)))
    (maphash 
     (lambda (k v)
       (declare (ignore k))
       (push (get-raceinfo v) races))
     *races-ht*)
    (with-output-to-string (s)
      (json s races))))

(defun get-raceinfo (race)
  (make-raceinfo
   :name (joref race "name")
   :id (joref race "objectId")
   :class (joref (joref race "polar") "classBoat")
   :start-time (joref (joref race "start") "iso")
   :start-pos (make-latlng
               :lat (joref (joref race "startLocation") "latitude")
               :lng (joref (joref race "startLocation") "longitude"))))

;;; The web page can't fetch the position from the Telnet port itself.
(defun |getBoatPosition| (handler request response &key (|host| "nmea.realsail.net") (|port| ""))
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
      (reset-nmea-listener (routing-nmea-connection routing) host port))
    (with-output-to-string (s)
      (json s
            (get-nmea-position (routing-nmea-connection routing) host port)))))

(defun |resetNMEAConnection| (handler request response &key (|host| "nmea.realsail.net") (|port| ""))
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
           (stop-nmea-listener (routing-nmea-connection routing) host port)
           (format nil "Stopped listening on ~a:~a" host port))
          (t
           (unless (ignore-errors
                    (numberp (parse-integer port)))
             (error "Invalid NMEA port ~a" port))
           (reset-nmea-listener (routing-nmea-connection routing) host port)
           (format nil "Listening on ~a:~a" host port)))))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

;;; Keep session table when reloading system!
(defvar *session-ht* (make-hash-table :test #'equalp))

(defstruct session
  (session-id (make-session-id))
  (routings (make-hash-table :test #'equal)))

(defun create-session (&key (session-id (make-session-id)) (race-id "default"))
  (let ((session (make-session :session-id session-id)))
    (setf (gethash race-id (session-routings session))
          (make-routing :race-id race-id))
    (values session)))

(defun session-routing (session race-id)
  (or
   (gethash race-id (session-routings session))
   (log2:info "Creating routing for session ~a race ~a" (session-session-id session) race-id)
   (setf (gethash race-id (session-routings session))
         (make-routing :race-id race-id))))

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
                (setf session (setf (gethash session-id *session-ht*)
                                    (create-session :session-id session-id :race-id race-id)))
                (log2:info "Session was lost for SessionID ~a." session-id))
               (t
                (setf session stored-session)
                (log2:info "Session retrieved for SessionID ~a." session-id)))))
          (t
           (setf session-id (make-session-id))
           (set-cookie response "SessionID" session-id :options '())
           (setf session (setf (gethash session-id *session-ht*)
                               (create-session :session-id session-id :race-id race-id)))
           (log2:info "Session created for new SessionID ~a." session-id)))
    session))


(defun load-file (path response)
  (with-open-file (f path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence buffer f)
      (setf (http-body response) buffer)))
  (setf (http-header response :|Content-Type|) "text/html"))


(defun get-routing-url (session race-id)
  (let ((routing (session-routing session race-id)))
    (format nil "/vh?~@{~a=~a~^&~}"
            "app" "main" 
            "race" race-id
            "forecastbundle" (routing-dataset routing)
            "starttime" (routing-starttime routing)
            "polars" (routing-polars routing)
            "options" (format nil "~{~a~^,~}" (routing-options routing))
            "minwind" (if (routing-minwind routing) "true" "false")
            "duration" (/ (routing-stepmax routing) 3600))))

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
                         (make-latlng :lat (read-from-string value)
                                      :lng (read-from-string startlon)))
                   result-pairs)))
          ((string= name "startlon")
           )
          (t
           (push pair result-pairs)))))))

(defun set-routing-parameter (session routing name value)
  (log2:info "Session ~a: ~a=~a" (session-session-id session) name value)
  (cond
    ((string= name "race")
     )
    ((or (string= name "starttime")
         (string= name "ts"))
     (setf (routing-starttime routing) value))
    ((or (string= name "cycle"))
     (cond
       ((null value)
        (setf (routing-cycle routing)
              nil))
       (t
        (assert (eql (length value)
                     (length "2020-10-24T00:00:00Z")))
        (setf (routing-cycle routing) value))))
    ((string= name "polars")
     (setf (routing-polars routing)  value))
    ((string= name "options")
     (setf (routing-options routing) (cl-utilities:split-sequence #\, value)))
    ((string= name "minwind")
     (setf (routing-minwind routing) (string= value "true")))
    ((string= name "duration")
     (let ((duration-hrs
            (read-from-string value)))
       (setf (routing-stepmax routing)
             (* duration-hrs 3600))))
    ((string= name "mode")
     (cond
       ((string= value "maxorigin")
        (setf (routing-mode routing) +max-origin+))
       ((string= value "mindestination")
        (setf (routing-mode routing) +min-destination+))
       ((string= value "convhull")
        (error "NYI"))
       (t
        (error "Invalid search mode"))))
    ((string= name "searchangle")
     (let ((fan (read-from-string value)))
       (setf (routing-fan routing) fan)))
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

