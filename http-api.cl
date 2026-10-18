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
                                              (|options| nil)
                                              |polarsId|
                                              (|useWaves| nil)
                                              (|currents| nil)
                                              (|forecast_model| nil)
                                              |slat|
                                              |slon|
                                              |dlat|
                                              |dlon|
                                              (|startTime| nil)
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
           (currents (read-arg |currents| 'symbol 'cl-weather))
           (forecast-model (let ((model (read-arg |forecast_model|)))
                             (if (and model (not (string= model "")))
                                 (read-arg |forecast_model| 'symbol 'cl-weather)
                                 'cl-weather::noaa-gfs-wind)))
           (routing
             (get-routing-presets :polars-id (decode-uri-component |polarsId|)
                                  :use-waves (string-equal (read-arg |useWaves|) "true")
                                  :currents currents
                                  :forecast-model forecast-model
                                  :options (cl-utilities:split-sequence #\, |options|)
                                  :starttime |startTime|
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
      
      (setf (http-header response :|Content-Type|)
            "application/json")
      (let ((routeinfo
              (get-route routing)))
        (log2:info "Status ~a ~a"
                   (routeinfo-status routeinfo)
                   (routeinfo-stats routeinfo))
        (values
         (with-output-to-string (s)
           (json s routeinfo)))))))

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
                              (polars-id)
                              (use-waves nil)
                              (currents nil)
                              (forecast-model 'cl-weather::noaa-gfs-wind)
                              (starttime nil)
                              (options)
                              (stepmax (* 24 60 60))
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
    (make-routing
     :fan *max-angle*
     :start start
     :dest dest 
     :box (make-routing-box start dest)
     :starttime starttime
     :stepmax (min (* *max-route-hours* 3600) stepmax)
     :options options
     :polars cpolars
     :use-waves use-waves
     :currents currents
     :forecast-model forecast-model
     :twa-angles (make-twa-angles-buffer cpolars))))

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
