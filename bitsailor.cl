;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2024-06-29 21:32:53>

(in-package :bitsailor)

;;; Avoid line-breaks when printing log messages

(log2:info "Heap space: ~a" (sb-ext:dynamic-space-size))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main configuration files
;;; - rcfile
;;; - server-config

(defvar *rcfile*  "bitsailor.conf")

(defvar *server-config* 
  (merge-pathnames (make-pathname :name "server-config" :type "cl")
                   *source-root*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stopping the router main function

(defvar *run* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Router main function
;;;
;;; 1 - Load *rcfile*, applying customizations
;;; 2 - Create or open user database
;;; 3 - Load map
;;; 4 - Load polars and races
;;; 5 - Start timer loop
;;; 6 - 
;;; 7 - Cleanup old weather data
;;; 8 - Start weather updates
;;; 9 - Configure and start web server from *server-config*   

(defun start-router (&key
                       (rcfile *rcfile*)
                       (resolution *resolutions*)
                       (start-sentinel t))
  (log2:info "Path: ~a " #.*compile-file-truename*)
  (let ((local-rcfile
          (make-pathname :name rcfile))
        (home-rcfile
          (merge-pathnames (make-pathname :name rcfile)
                           (user-homedir-pathname))))
    (cond
      ((probe-file local-rcfile)
       (log2:info "Loading ~a " local-rcfile)        
       (load local-rcfile :verbose t :print t))
      ((probe-file home-rcfile)
       (log2:info "Loading ~a " home-rcfile)        
       (load home-rcfile :verbose t :print t))
      (t
       (log2:warning "Found neither ~a nor ~a" local-rcfile home-rcfile)))

    (multiple-value-bind
          (success error)
        (ignore-errors
         (deploy))
      (unless success
        (log2:warning "~a" error)))
    (log2:info "Opening database ~a" *db*)
    (setf *dbcon* (sqlite-client:%connect% *db* sqlite-client::+sqlite_open_fullmutex+))

    ;; The GSHHS coastline is abysmally slow
    ;; (ensure-map :filename "/home/michael/Maps/GSHHS/GSHHS_shp/h/GSHHS_h_L1.shp")
    (if *use-bitmap*
        (ensure-bitmap)
        (ensure-map))
    
    (load-polars-directory)
    (load-race-definitions :directory *races-dir*)

    ;; Start timers
    (timers:start-timer-loop)

    (setf *statistics-timer*
          (timers:add-timer (lambda ()
                              (update-statistics))
                            :id (format nil "UPDATE-STATISTICS")
                            :hours nil
                            :minutes nil))
    
    ;; Cleanup old forecasts once (also called periodically from cleanup timer)
    (cleanup-cycles :dry-run nil)

    ;; Load latest complete bundle and possbily update (synchronous), start asynchronous updates.
    (log2:info "Downloading forecasts and starting weather updates")
    (bordeaux-threads:make-thread (lambda ()
                                    (start-cycle-updates :resolution resolution))
                                  :name "GFS-DOWNLOAD")
    
    ;; Start web server
    (log2:info "Starting web server ~a" *server-config*)
    (polarcl:load-configuration *server-config*)

    ;; Prevent function from exiting, this is our toplevel
    (when start-sentinel
      (flet ((sentinel ()
               (loop
                 :while *run*
                 :do (progn
                       (log2:info "Keeping toplevel alive")
                       (log2:trace "Threads:~%~{~34T~a~%~}" (bordeaux-threads:all-threads))
                       (sleep 30)))
               (log2:warning "Server terminated, destroying all threads")
               (sleep 1)
               (map nil
                    (lambda (thread)
                      (let ((current (bordeaux-threads:current-thread)))
                        (unless (eq thread current)
                          (bordeaux-threads:destroy-thread thread))))
                    (bordeaux-threads:all-threads))
               (log2:warning "Exiting.")))
        (setf *run* t)
        (sentinel)))))

(defvar *races-ht* (make-hash-table :test #'equalp))

(defvar +races-ht-lock+
  (bordeaux-threads:make-lock "races-ht"))

(defun race-info-is-gfs025 (race-info)
  (string= (joref (race-info-data race-info) "gfs025") "TRUE"))

(defun race-info-resolution (race-info)
  (if (race-info-is-gfs025 race-info)
                   "0p25"
                   "1p00"))

(defun race-info (race-id)
  (bordeaux-threads:with-lock-held (+races-ht-lock+)
    (gethash race-id *races-ht*)))

(defsetf race-info set-race-info)

(defun set-race-info (race-id info)
  (bordeaux-threads:with-lock-held (+races-ht-lock+)
    (setf (gethash race-id *races-ht*) info)))

(defun map-races (function)
  (bordeaux-threads:with-lock-held (+races-ht-lock+)
    (maphash function *races-ht*)))

(defun load-race-definitions (&key (directory *races-dir*))
  (log2:info "Loading races from URL")
  (let ((races (get-leg-descriptions)))
    (bordeaux-threads:with-lock-held (+races-ht-lock+)
      (clrhash *races-ht*)
      (store-race-data-vr races))))

(defun get-leg-descriptions ()
  (let* ((response
          (curl:http "https://prod.vro.sparks.virtualregatta.com/rs/device/Xcl3WbCUmfcu5pWCktUoC0slGT4xkbEt/DeviceAuthenticationRequest"
                     :method :post
                     :body "{\"@class\":\"DeviceAuthenticationRequest\",\"deviceId\":\"3d87c3bb-8bf9-4d79-a0b3-4ed178287a35\",\"deviceOS\":\"WEBGL\"}"))
         (body (parse-json (curl::http-response-body response)))
         (token (joref body "authToken"))
         (user-id (joref body "userId"))
         (r 621356040007290000)
         (d (+ r (* 10000 (timestamp-to-unix (now))))))
    (let* ((response
             (event-request :|authToken| token
                            :|playerId| user-id
                            :|requestId|  (format nil "~a_1" d)
                            :|eventKey| "Leg_GetList"))
           (leglist (joref
                     (joref (parse-json (curl::http-response-body response))
                            "scriptData")
                     "res"))
           (legids (loop
                     :for legdef :across leglist
                     :collect (list (joref legdef "raceId")
                                    (joref legdef "legNum")
                                    (joref legdef "raceName")))))
      (loop
        :for (race-id leg-num &rest _) :in legids
        :for k :from 2
        :collect (curl::http-response-body
                  (event-request :|authToken| token
                                 :|playerId| user-id
                                 :|requestId|  (format nil "~a_~a" d k)
                                 :|eventKey| "Leg_GetInfo"
                                 :|race_id| race-id
                                 :|leg_num| leg-num))))))

(defun event-request (&rest key-pairs &key (|@class| "LogEventRequest") |authToken| |playerId| |requestId| |eventKey| &allow-other-keys)
  (let ((body
          (format nil "{~{\"~a\": \"~a\"~^, ~}}" (list* :|@class| |@class| :|authToken| |authToken| :|playerId| |playerId| :|requestId| |requestId| :|eventKey| |eventKey| key-pairs))))
    (curl:http "https://prod.vro.sparks.virtualregatta.com/rs/device/Xcl3WbCUmfcu5pWCktUoC0slGT4xkbEt/LogEventRequest"
               :method :post
               :body body)))

(defun update-statistics ()
  (let ((lowbound (local-time:adjust-timestamp (local-time:now) (offset :minute -10))))
    (bordeaux-threads:with-lock-held (+last-request-lock+)
      (setf *last-request*
            (remove-if (lambda (entry)
                         (local-time:timestamp< (car entry) lowbound))
                       *last-request*)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
