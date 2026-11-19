;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-04-20 20:38:46>

(in-package :bitsailor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP API

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic handlers

;;; This function is called from a dynamic handler
;;; Serve startpage.html from the configured web root; log failures and set HTTP status 500.
(defun start-page (server handler request response)
  (declare (ignore server handler request))
  ;; The HTML selects versioned UI assets and must be revalidated after updates.
  (setf (http-header response :|Cache-Control|) "no-cache")
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
;;; Promote the account matching the URL activation secret from USER_PROV to USER and serve the
;;; activation page.
(defun activate-account (server handler request response)
  (declare (ignore server handler request))
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

;;; Reserve a boat name and send an activation email; conflicting names produce an HTTP 400
;;; response.
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

;;; Decode each percent-encoded hexadecimal character; this does not perform UTF-8 byte decoding
;;; or translate plus signs.
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

;;; Parse an optional ISO UTC forecast cycle and reject invalid source run hours.
(defun parse-routing-cycle (value model)
  (when (and value (not (string= value "")))
    (handler-case
        (let* ((time (local-time:parse-timestring (decode-uri-component value)))
               (cycle (cl-weather::make-cycle% :timestamp time)))
          (unless (and (zerop (timestamp-minute time :timezone +utc-zone+))
                       (zerop (timestamp-second time :timezone +utc-zone+))
                       (zerop (local-time:nsec-of time))
                       (member (timestamp-hour time :timezone +utc-zone+)
                               (cl-weather::schedule-runs (cl-weather::datasource-schedule-for-cycle model cycle))))
            (error "Invalid run time"))
          cycle)
      (error () (error 'request-error :message "Invalid forecast cycle; use an ISO UTC source run time")))))

(defun parse-current-source (value)
  "Accept supported current sources or NONE; reject retired sources in saved API requests."
  (unless (or (null value) (string= value "") (string-equal value "NONE"))
    (let ((source (read-arg value 'symbol 'cl-weather)))
      (unless (member source '(cl-weather:ecmwf-ifs-current cl-weather::fw-current-agulhas
                               cl-weather::fw-current-english-channel))
        (error 'request-error :message "Unsupported current source; choose IFS, Agulhas or English Channel (IBI)"))
      source)))

;;; Parse routing query arguments, validate start/waypoints, run the router and serialize JSON;
;;; request errors map to 400 and other errors to 500.
(defun |getRoute| (handler request response &key
                                              (|options| nil)
                                              |polarsId|
                                              (|useWaves| nil)
                                              (|fine_tune| nil)
                                              (|search_box| nil)
                                              (|currents| nil)
                                              (|forecast_model| nil)
                                              (|secondary_forecast_model| nil)
                                              (|forecast_switch_hours| nil)
                                              (|cycle| nil)
                                              (|stepper| nil)
                                              (|reverse| nil) (|reverse_percent| nil)
                                              |slat|
                                              |slon|
                                              |dlat|
                                              |dlon|
                                              (|waypoints| nil)
                                              (|startTime| nil)
                                              (|duration| (* *max-route-hours* 3600)
                                                          duration-supplied-p))
  (declare (ignore handler request))
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
           (currents (parse-current-source |currents|))
           (forecast-model (let ((model (read-arg |forecast_model|)))
                             (if (and model (not (string= model "")))
                                 (read-arg |forecast_model| 'symbol 'cl-weather)
                                 'cl-weather:noaa-gfs-wind)))
           (routing
             (get-routing-presets :polars-id (decode-uri-component |polarsId|)
                                  :use-waves (string-equal (read-arg |useWaves|) "true")
                                  :fine-tune (string-equal (read-arg |fine_tune|) "true")
                                  :use-box (not (string-equal (read-arg |search_box|) "false"))
                                  :currents currents
                                  :forecast-model forecast-model
                                  :secondary-forecast-model (parse-secondary-model |secondary_forecast_model| |forecast_switch_hours|)
                                  :forecast-switch-hours (parse-forecast-switch-hours |forecast_switch_hours|)
                                  :cycle (parse-routing-cycle |cycle| forecast-model)
                                  :stepper (parse-routing-stepper |stepper|)
                                  :reverse-percent (parse-reverse-options |reverse| |reverse_percent|)
                                  :options (cl-utilities:split-sequence #\, |options|)
                                  :starttime |startTime|
                                  :stepmax (if duration-supplied-p
                                               (read-arg |duration|)
                                               |duration|)
                                  :slat (read-arg |slat| 'double-float)
                                  :slon (read-arg |slon| 'double-float)
                                  :dlat (read-arg |dlat| 'double-float)
                                  :dlon (read-arg |dlon| 'double-float))))
      (setf (routing-waypoints routing) (parse-waypoints |waypoints|))
      (when (and (routing-reverse-percent routing) (routing-waypoints routing))
        (error 'request-error :message "Reverse routing requires a route without waypoints or gates"))
      (when (cl-map:point-on-land-p (routing-start routing))
        (error 'request-error :message "Start point is on land"))
      (dolist (waypoint (routing-waypoints routing))
        ;; Gate endpoints may lie on the coast, so only points are checked
        (when (and (not (gate-p waypoint)) (cl-map:point-on-land-p waypoint))
          (error 'request-error
                 :message (format nil "Waypoint ~a is on land" (format-latlng nil waypoint)))))
      (when (equalp (routing-start routing) (routing-dest routing))
        (error 'request-error :message "Start and destination are the same"))
      
      (setf (http-header response :|Content-Type|)
            "application/json")
      (setf (http-header response :|Cache-Control|) "no-store"
            (http-header response :|Vary|) "Accept-Encoding"
            ;; Temporary diagnostic for truncated gzip route responses.
            (http-header response :|Content-Encoding|) "identity")
      (let ((routeinfo
              (get-route routing)))
        (log2:info "Status ~a ~a"
                   (routeinfo-status routeinfo)
                   (routeinfo-stats routeinfo))
        ;; Routes finishing before the next hour legitimately have no isochrones.
        ;; A vector serializes as [] even when empty; Lisp NIL serializes as false.
        (setf (routeinfo-isochrones routeinfo) (coerce (routeinfo-isochrones routeinfo) 'vector))
        (let ((bytes (babel:string-to-octets
                       (with-output-to-string (s) (json s routeinfo)) :encoding :utf-8)))
          ;; Kept separate from Content-Length, which may describe a gzip body.
          (setf (http-header response :|X-Route-Bytes|) (length bytes))
          (log2:info "Serialized route response: ~d UTF-8 bytes" (length bytes))
          bytes)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands

;;; Set permissive CORS headers and a 204 response for an OPTIONS request.
(defun handle-options (server handler request response)
  (declare (ignore server handler request))
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

;;; Forward a JSON command body to the Virtual Regatta device endpoint and copy its response body.
(defun execute-commands (server handler request response)
  (declare (ignore server handler request))
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

;;; Serialize datasource metadata used by the browser to locate forecast files.
(defun |getServerSettings| (handler request response &key |forecast_model| |cycle|)
  (declare (ignore handler request))
  (handler-case
      (let* ((model (when |forecast_model| (read-arg |forecast_model| 'symbol 'cl-weather)))
             (cycle (when |cycle| (parse-routing-cycle |cycle| model))))
        (when (and |cycle| (null model))
          (error 'request-error :message "forecast_model is required with cycle"))
        (with-output-to-string (s)
          ;; Describe the exact run, including archived runs; never substitute a cycle here.
          (json s (if cycle
                      (make-server-settings :datasources (list (cl-weather::datasource-info-for-cycle model cycle)))
                      (get-server-settings)))))
    (request-error (condition)
      (setf (status-code response) 400)
      (format nil "~a" (message condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Current display

(defstruct current-field source time cycle columns rows vectors)

(defun current-display-field (source time north south west east columns rows)
  "Sample regional currents at viewport cell centres in Web Mercator.
Vectors are east/north flow components in m/s; zero vectors cover calm or
out-of-domain cells and land. Use the same temporal/spatial interpolation as
routing, then mask arrow centres against the router's coastline geometry."
  (let* ((params (get-params source time))
         (top (log (tan (+ (/ pi 4) (/ (rad north) 2)))))
         (bottom (log (tan (+ (/ pi 4) (/ (rad south) 2)))))
         (vectors (make-array (* rows columns))))
    (dotimes (row rows)
      (let* ((y (+ top (* (/ (+ row 0.5d0) rows) (- bottom top))))
             (lat (deg (- (* 2 (atan (exp y))) (/ pi 2)))))
        (dotimes (column columns)
          (let ((lon (normalize-angle (+ west (* (/ (+ column 0.5d0) columns) (- east west))))))
            (multiple-value-bind (direction speed u v) (interpolate-uv lat lon params)
              (declare (ignore direction speed))
              ;; Zeroing missing GRIB nodes does not stop bilinear interpolation
              ;; from spreading an offshore vector inland across a coastal cell.
              (when (and (or (/= u 0d0) (/= v 0d0))
                         (let ((position (make-latlng :lat lat :lng lon)))
                           (if *use-bitmap* (bm-is-land position)
                               (cl-map:point-on-land-p position))))
                (setf u 0d0 v 0d0))
              (setf (aref vectors (+ (* row columns) column)) (vector u v)))))))
    (make-current-field :source source :time time
                        :cycle (cl-weather:uv-cycle (params-fc1 params))
                        :columns columns :rows rows :vectors vectors)))

(defun |getCurrentField| (handler request response &key |source| |forecast_model| |time| |north| |south| |west| |east| |columns| |rows|)
  "Return a bounded viewport current grid for the selected source and display time."
  (declare (ignore handler request |forecast_model|))
  (setf (http-header response :|Content-Type|) "application/json"
        (http-header response :|Cache-Control|) "no-store")
  (handler-case
      (let* ((source (parse-current-source |source|))
             (time (handler-case (local-time:parse-timestring (decode-uri-component |time|))
                     (error () (error 'request-error :message "Invalid current display time"))))
             (north (read-arg |north| 'double-float))
             (south (read-arg |south| 'double-float))
             (west (read-arg |west| 'double-float))
             (east (read-arg |east| 'double-float))
             (columns (parse-integer |columns|))
             (rows (parse-integer |rows|)))
        (unless (and source
                     (<= -85.051129d0 south north 85.051129d0)
                     (< south north) (< west east) (<= (- east west) 1080d0)
                     (<= 1 columns 256) (<= 1 rows 256) (<= (* columns rows) 32768))
          (error 'request-error :message "Invalid current source, viewport or grid size"))
        (with-output-to-string (s)
          (json s (current-display-field source time north south west east columns rows))))
    ((or request-error parse-error type-error) (e)
      (setf (status-code response) 400)
      (format nil "~a" e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polars

;;; Serialize the available polar IDs and labels for the browser.
(defun |getPolarsList| (handler request response)
  (declare (ignore handler request response))
  (with-output-to-string (s)
    (json s (get-polars-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NMEA

;;; The web page can't fetch the position from the Telnet port itself.
;;; Validate the supplied NMEA port, fetch a GPRMC position server-side and serialize the result.
(defun |getBoatPosition| (handler request response &key (|host| "nmea.fairwinds.world") (|port| ""))
  (declare (ignore handler request response))
  (let* ((host |host|)
         (port |port|))
    (unless (ignore-errors
             (numberp (parse-integer port)))
      (error "Invalid NMEA port ~a" port))
    (with-output-to-string (s)
      (json s
            (get-gprmc host port)))))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Router status & stats

(defstruct downloadstatus
  name
  resolution
  ;; Base time of the newest cycle that was downloaded completely, or NIL
  completed
  ;; Base time of a newer cycle with a partial download, or NIL
  progress
  progress-done
  progress-total)

(defparameter *download-status-sources*
  '(("GFS" cl-weather::noaa-gfs-wind)
    ("IFS" cl-weather::ecmwf-ifs-wind)
    ("AIFS-SINGLE" cl-weather::ecmwf-aifs-wind)
    ("IFS currents" cl-weather:ecmwf-ifs-current)
    ("AROME" cl-weather::arome-wind)
    ("Fairwinds waves" cl-weather::gfswave-combined)
    ("Fairwinds Agulhas" cl-weather::fw-current-agulhas)
    ("Fairwinds English Channel (IBI)" cl-weather::fw-current-english-channel))
  "Display name and the datasources that make up each download indicator")

(defun cycle-download-count (ids cycle)
  "Number of downloaded files and total number of files of CYCLE for all datasources IDS.
Returns two values, present and total; returns NIL on errors. Missing files count as absent."
  (handler-case
      (let ((present 0) (total 0))
        (dolist (id ids)
          (let* ((datasource (cl-weather::get-datasource id cycle))
                 (paths (remove-duplicates
                         (mapcar (lambda (step) (cl-weather::local-pathname datasource step))
                                 (cl-weather::schedule-steps (cl-weather::schedule datasource)))
                         :test #'equal))
                 (full-file-p (typep datasource 'cl-weather::file-download)))
            (incf total (length paths))
            (dolist (path paths)
              (when (and (probe-file path)
                         ;; Single-file downloads are written in place, so check for completeness
                         (or (not full-file-p)
                             (ignore-errors (cl-weather::grib-file-complete-p path))))
                (incf present)))))
        (values present total))
    (error () nil)))

;;; Inspect up to twelve cycles per datasource group, reporting the latest complete cycle and a
;;; newer partial download.
(defun get-download-status ()
  (loop
    :for (name . ids) :in *download-status-sources*
    :collect
    (let ((completed nil) (progress nil) (done 0) (total 0))
      (loop
        :with cycle = (cl-weather:current-cycle (first ids))
        :repeat 12
        :do (multiple-value-bind (present all) (cycle-download-count ids cycle)
              (cond
                ((null present))
                ((and (> all 0) (= present all))
                 (setf completed (cl-weather:cycle-timestamp cycle))
                 (return))
                ((and (> present 0) (null progress))
                 (setf progress (cl-weather:cycle-timestamp cycle)
                       done present
                       total all))))
            (setf cycle (cl-weather:previous-cycle (first ids) cycle)))
      (make-downloadstatus :name name :completed completed :progress progress
                           :resolution (download-grid-resolution (first ids) (or completed progress))
                           :progress-done done :progress-total total))))

(defstruct (routerstatus (:constructor routerstatus
                             (server-start
                              downloads
                              fc-cache-size 
                              requestcount
                              max_iso_points
                              twa_steps
                              last_routestats &optional parameters memory caches)))
  server-start
  downloads
  fc-cache-size
  requestcount
  max_iso_points
  twa_steps
  last_routestats
  parameters memory caches)

;;; Serialize activity, downloads, cache counts, memory snapshots, current operational settings
;;; and the last route statistics; keep legacy top-level fields for existing clients.
(defun |getStatistics| (handler request response)
  (declare (ignore handler request response))
  (let ((memory (get-memory-status)))
    (with-output-to-string (s)
      (json s
            (routerstatus *server-start-time*
                          (get-download-status)
                          (memorystatus-forecast-entries memory)
                          (bordeaux-threads:with-lock-held (+last-request-lock+) (length *last-request*))
                          *max-iso-points*
                          *twa-steps*
                          (bordeaux-threads:with-lock-held (+last-routestats-lock+) *last-routestats*)
                          (get-status-parameters) memory (get-cache-status))))))

;;; Count entries by traversing the supplied hash table.
(defun number-of-entries (hashtable)
  (let ((count 0))
    (maphash (lambda (k v) (declare (ignore k v)) (incf count)) hashtable)
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

;;; Read PATH as octets into the HTTP response body and infer Content-Type from its extension.
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
;;; Secondary model switching is opt-in; reject malformed enabled requests as 400.
(defun parse-forecast-switch-hours (value)
  (cond ((or (null value) (equal value "off")) nil)
        ((member value '("6" "12" "24") :test #'equal) (parse-integer value))
        (t (error 'request-error :message "forecast_switch_hours must be off, 6, 12 or 24"))))

(defun parse-secondary-model (value hours)
  (when (parse-forecast-switch-hours hours)
    (let ((model (and value (find-symbol (string-upcase value) :cl-weather))))
      (unless (and model (find-class model nil) (subtypep model 'cl-weather::datakind-wind))
        (error 'request-error :message "A valid secondary wind model is required"))
      model)))

;;; Helper functions

(defun parse-reverse-options (enabled percent)
  "Return the passage-time allowance, or NIL for an ordinary request."
  (unless (member enabled '(nil "true" "false") :test #'equalp)
    (error 'request-error :message "reverse must be true or false"))
  (let ((value (if percent
                   (and (plusp (length percent))
                        (cl-ppcre:scan "\\A[+]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][+-]?[0-9]+)?\\z" percent)
                        (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
                          (multiple-value-bind (n end) (ignore-errors (read-from-string percent))
                            (and (realp n) (= end (length percent)) n))))
                   2d0)))
    (unless (and value (<= 0 value most-positive-double-float))
      (error 'request-error :message "reverse_percent must be a finite nonnegative percentage"))
    (when (equalp enabled "true") (coerce value 'double-float))))

;;; Accept only the two public step schedules; older requests default to standard.
(defun parse-routing-stepper (value)
  (cond ((or (null value) (string= value "") (string-equal value "standard")) :standard)
        ((string-equal value "hires") :hires)
        (t (error 'request-error :message "stepper must be standard or hires"))))

;;; Build routing inputs, polar cache and search bounds; cap duration in seconds at *MAX-ROUTE-
;;; HOURS*.
(defun get-routing-presets (&key
                              (polars-id)
                              (use-waves nil)
                              (fine-tune nil)
                              (use-box t)
                              (currents nil)
                              (forecast-model 'cl-weather:noaa-gfs-wind)
                              (secondary-forecast-model nil)
                              (forecast-switch-hours nil)
                              (cycle nil)
                              (starttime nil)
                              (options)
                              (stepmax (* 24 60 60))
                              (stepper :standard)
                              reverse-percent
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
    (when starttime (setf starttime (routing-minute-ceiling (parse-datetime-local starttime))))
    (make-routing
     :fan *max-angle*
     :start start
     :dest dest 
     :use-box use-box
     :box (when use-box (make-routing-box start dest))
     :starttime starttime
     :stepmax (min (* *max-route-hours* 3600) stepmax)
     :stepper stepper
     :reverse-percent reverse-percent
     :stepper-starttime (when starttime (parse-datetime-local starttime))
     :options options
     :polars cpolars
     :use-waves use-waves
     :fine-tune fine-tune
     :currents currents
     :forecast-model forecast-model
     :secondary-forecast-model secondary-forecast-model
     :forecast-switch-time (when forecast-switch-hours
                             (adjust-timestamp (parse-datetime-local starttime)
                                               (:offset :hour forecast-switch-hours)))
     :cycle cycle
     :twa-angles (make-twa-angles-buffer cpolars))))

(defun parse-waypoints (string)
  "Parse waypoints separated by ~. Each is either lat,lon (point) or
lat1,lon1,lat2,lon2 (gate). Returns a list of latlng and GATE structs."
  (when (and string (string/= string ""))
    (loop
      :for item :in (cl-utilities:split-sequence #\~ string)
      :for coords = (mapcar (lambda (s) (read-arg s 'double-float))
                            (cl-utilities:split-sequence #\, item))
      :do (unless (and (member (length coords) '(2 4)) (every #'numberp coords))
            (error 'request-error :message "Invalid waypoints"))
      :collect (if (= (length coords) 2)
                   (make-latlng :lat (first coords) :lng (second coords))
                   (make-gate :a (make-latlng :lat (first coords) :lng (second coords))
                              :b (make-latlng :lat (third coords) :lng (fourth coords)))))))

;;; Copy the polar's angle samples into an adjustable double-float vector with two slots reserved
;;; for runtime VMG angles.
(defun make-twa-angles-buffer (cpolars)
  (let ((buffer
          (make-array (length (cpolars-twa cpolars))
                      :element-type 'double-float
                      :adjustable t
                      :fill-pointer t
                      :initial-contents (cpolars-twa cpolars))))
    (vector-push-extend 0d0 buffer)
    (vector-push-extend 0d0 buffer)
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined races

;;; Read a UUID from Linux's /proc/sys/kernel/random/uuid interface.
(defun create-uuid ()
  (with-open-file (f "/proc/sys/kernel/random/uuid")
    (read-line f)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
