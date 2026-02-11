;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-02-07 21:36:09>

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

(defun |getRoute| (handler request response &key
                                              (|raceId| nil)
                                              (|options| nil)
                                              (|tack| nil)
                                              (|sail| nil)
                                              (|resolution| "0p25")
                                              |polarsId|
                                              (|useECMWF| nil)
                                              (|currents| nil)
                                              |slat|
                                              |slon|
                                              |dlat|
                                              |dlon|
                                              (|startTime| nil)
                                              (|cycleTS| nil cycle-supplied-p)
                                              (|duration| (* *max-route-hours* 3600)
                                                          duration-supplied-p))
  (handler-bind
      ((request-error
         (lambda (c)
           (log2:warning "~a" c)
           (setf (status-code response) 400)
           (setf (status-text response) (message c))
           (return-from |getRoute| (format nil "~a" (message c)))))
       (error
         (lambda (e)
           (log2:error "~a" e)
           (sb-debug:print-backtrace)
           (setf (status-code response) 500)
           (setf (status-text response) (format nil "~a" e))
           (return-from |getRoute| (format nil "~a" e)))))

    (let* ((*read-default-float-format* 'double-float)
           (cycle (if cycle-supplied-p
                      (make-cycle :timestamp (parse-datetime |cycleTS|))
                      (cl-weather::timestamp-cycle 'cl-weather::noaa-gfs-wind (now))))
           (currents (read-arg |currents| 'symbol 'cl-weather))
           (routing
             (get-routing-presets :race-id |raceId|
                                  :polars-id (decode-uri-component |polarsId|)
                                  :use-ecmwf (string-equal (read-arg |useECMWF|) "true")
                                  :currents currents
                                  :options (cl-utilities:split-sequence #\, |options|)
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
                                  :dlon (read-arg |dlon| 'double-float))))
      (when (cl-map:point-on-land-p (routing-start routing))
        (error 'request-error :message "Start point is on land"))
      (when (equalp (routing-start routing) (routing-dest routing))
        (error 'request-error :message "Start and destination are the same"))
      (let ((routeinfo
              (get-route routing)))
        (log2:info "Status ~a ~a"
                   (routeinfo-status routeinfo)
                   (routeinfo-stats routeinfo))
        (values
         (with-output-to-string (s)
           (json s routeinfo)))))))

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

(defun arc-length (a b)
  (when (< a 0) (incf a 360))
  (when (< b 0) (incf b 360))
  (when (< b a) (incf b 360))
  (- b a))

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
               (get-routing-presets :race-id |raceId|
                                    :polars-id |polarsId|
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
;;; Server Settings

(defun |getServerSettings| (handler request response)
  ;; Provide datasource path templates etc.
  (with-output-to-string (s)
    (json s (get-server-settings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polars

(defun |getPolarsList| (handler request response)
  (with-output-to-string (s)
    (json s (get-polars-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NMEA

;;; The web page can't fetch the position from the Telnet port itself.
(defun |getBoatPosition| (handler request response &key (|host| "nmea.fairwinds.world") (|port| ""))
  (let* ((host |host|)
         (port |port|))
    (unless (ignore-errors
             (numberp (parse-integer port)))
      (error "Invalid NMEA port ~a" port))
    (with-output-to-string (s)
      (json s
            (get-gprmc host port)))))
           
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
  "[]")

(defun get-raceinfo (race)
  (let* ((id (joref race "_id"))
         (boat (joref race "boat"))
         (start (joref race "start")))
    (make-raceinfo
     :type "vr"
     :name (joref race "name")
     :id (format nil "~a.~a" (joref id "race_id") (or (joref id "num")
                                                      (joref id "leg_num")))
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

(defstruct (routerstatus (:constructor routerstatus
                             (server-start
                              latest-forecast 
                              fc-cache-size 
                              requestcount
                              datasource
                              max_iso_points
                              twa_steps
                              last_routestats)))
  server-start
  latest-forecast
  fc-cache-size
  requestcount
  datasource
  max_iso_points
  twa_steps
  last_routestats)

(defun |getStatistics| (handler request response)
  (with-output-to-string (s)
    (json s
          (routerstatus *server-start-time*
                        cl-weather::*latest-forecast*
                        (number-of-entries cl-weather::*forecast-ht*)
                        (length *last-request*)
                        "NOAA"
                        *max-iso-points*
                        *twa-steps*
                        *last-routestats*))))

(defun number-of-entries (hashtable)
  (let ((count 0))
    (maphash (lambda (k v) (declare (ignore k v)) (incf count)) hashtable)
    count))

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
;;; Helper functions

(defun get-routing-presets (&key
                              (race-id)
                              (polars-id)
                              (use-ecmwf t)
                              (currents nil)
                              (starttime nil)
                              (resolution "0p25" resolution-provided-p)
                              (options)
                              (tack)
                              (sail nil)
                              (stepmax (* 24 60 60))
                              (cycle (current-cycle 'noaa-gfs-wind))
                              (slat)
                              (slon)
                              (dlat)
                              (dlon))
  (let* ((start (when (and slat slon) (make-latlng :lat slat :lng slon)))
         (dest (when (and dlat dlon) (make-latlng :lat dlat :lng dlon)))
         (polars-id (format nil "~a" polars-id))
         (options
           (or options
               '("hull" "foil" "winch" "heavy" "light" "reach")))
         (cpolars (get-combined-polars polars-id (encode-options options))))
    (when (not resolution-provided-p)
      (setf resolution "0p25"))
    (make-routing
     :fan *max-angle*
     :start start
     :dest dest 
     :box (make-routing-box start dest)
     :starttime starttime
     :stepmax (min (* *max-route-hours* 3600) stepmax)
     :options options
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
     :minwind 0d0
     :grib-source  :noaa
     :interpolation :bilinear
     :polars cpolars
     :use-ecmwf use-ecmwf
     :currents currents
     :twa-angles (make-twa-angles-buffer cpolars)
     :cycle cycle
     :merge-start *merge-start*
     :merge-window *merge-duration*
     :simplify-route nil)))

(defun make-twa-angles-buffer (cpolars)
  (let ((buffer
          (make-array (length (cpolars-twa cpolars))
                      :adjustable t
                      :fill-pointer t
                      :initial-contents (cpolars-twa cpolars))))
    (vector-push-extend 0d0 buffer)
    (vector-push-extend 0d0 buffer)
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined races

(defun create-uuid ()
  (with-open-file (f "/proc/sys/kernel/random/uuid")
    (read-line f)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
