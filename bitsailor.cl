;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2024-02-06 21:07:36>

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
;;; 6 - Start NMEA listener
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

    (setf *racelist-timer*
          (timers:add-timer (lambda ()
                              (fetch-rs-race-definitions))
                            :id (format nil "UPDATE-RACELIST")
                            :hours nil
                            :minutes '(0 15 30 45)))

    (setf *statistics-timer*
          (timers:add-timer (lambda ()
                              (update-statistics))
                            :id (format nil "UPDATE-STATISTICS")
                            :hours nil
                            :minutes nil))

    ;; NMEA
    (restart-nmea-listener-loop)
    
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
  (let ((path (merge-pathnames directory (make-pathname :name :wild :type "json"))))
    (log2:info "Loading races from ~a" path)
    (bordeaux-threads:with-lock-held (+races-ht-lock+)
      (clrhash *races-ht*)
      (loop
        :for name :in (directory path)
        :do (let ((json-object (parse-json-file name)))
              (cond
                ((typep json-object 'array)
                 (store-race-data-vr json-object))
                (T
                 (log2:warning "Skipping ~a" name)))))))
  (ignore-errors
   (bordeaux-threads:make-thread (lambda () 
                                   (fetch-rs-race-definitions))
                                 :name "FETCH-RS-RACES")))

(defun fetch-rs-race-definitions ()
  (let* ((response (curl:http "https://frontend.realsail.net/classes/Race"
                              :headers '(("accept" "*/*")
                                         ("accept-language" "en-US,en")
                                         ("content-type" "application/json")
                                         ("referer" "https://bitsailor.net/"))
                              :body "{\"where\":{\"visible\":true,\"version\":3},\"limit\":9007199254740991,\"order\":\"start\",\"_method\":\"GET\",\"_ApplicationId\":\"JFvkachvtsjN4f1glZ1ZHiGrchnkyeFtY9gNWbN4\",\"_JavaScriptKey\":\"e4QehnvfIIA1nrH0wc35onJkMq3fZA09uMNad0Ct\",\"_ClientVersion\":\"js2.19.0\",\"_InstallationId\":\"b1e14dd0-b68e-4cc0-9917-b47168ad350e\"}"))
         (json-object (parse-json (curl:http-response-body response))))

    (loop
      :for race-def :across (joref json-object "results")
      :do
         (let* ((polars (joref race-def "polar"))
                (polars-id (joref  polars "objectId")))
           (setf (joref polars "classBoat") (polars-label (get-polars-by-id polars-id)))))

    (bordeaux-threads:with-lock-held (+races-ht-lock+)
      (maphash (lambda (key entry)
                 (when (typep entry 'race-info-rs)
                   (remhash key *races-ht*)))
               *races-ht*)
      (store-race-data-rs json-object))))


(defun update-statistics ()
  (let ((lowbound (local-time:adjust-timestamp (local-time:now) (offset :minute -10))))
    (bordeaux-threads:with-lock-held (+last-request-lock+)
      (setf *last-request*
            (remove-if (lambda (entry)
                         (local-time:timestamp< (car entry) lowbound))
                       *last-request*)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
