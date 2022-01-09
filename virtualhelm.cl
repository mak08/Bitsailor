;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2022-01-09 23:49:34>

(in-package :virtualhelm)

;;; Avoid line-breaks when printing log messages
(setf *print-pretty* nil)

(log2:info "Heap space: ~a" (sb-ext:dynamic-space-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function

(defvar *server-config* 
  (merge-pathnames (make-pathname :name "server-config" :type "cl")
                   *source-root*))

(defun run-virtual-helm (&key
                           (rcfile ".vhrc")
                           (resolution  '("1p00" "0p25"))
                           (start-sentinel t))
  (log2:info "Path: ~a " #.*compile-file-truename*)
  (let ((rcfile
          (merge-pathnames (make-pathname :name rcfile)
                           (user-homedir-pathname))))
    (log2:info "Opening database ~a" *db*)
    (setf *dbcon* (sqlite-client:%connect% *db* sqlite-client::+sqlite_open_fullmutex+))
    (cond
      ((probe-file rcfile)
       (log2:info "Loading ~a " rcfile)        
       (load rcfile :verbose t :print t))
      (t
       (log2:warning "~a does not exist" rcfile)))

    ;; The GSHHS coastline is abysmally slow
    ;; (ensure-map :filename "/home/michael/Maps/GSHHS/GSHHS_shp/h/GSHHS_h_L1.shp")
    (ensure-map)
    (ensure-bitmap)
    
    (load-polars-directory)

    ;; Start timers
    (timers:start-timer-loop)

    ;; Cleanup old forecasts once (also called periodically from cleanup timer)
    (cleanup-cycles)

    ;; Load latest complete bundle and possbily update (synchronous), start asynchronous updates.
    (log2:info "Backfilling weather data and starting weather updates")
    (bordeaux-threads:make-thread (lambda ()
                                    (noaa-start-updates :resolution resolution))
                                  :name "GFS-DOWNLOAD")
    
    ;; Start web server
    (log2:info "Starting web server ~a" *server-config*)
    (polarcl:load-configuration *server-config*)

    ;; Keep function alive, this is our toplevel
    (flet ((sentinel ()
             (loop
               (progn
                 (log2:info "Keeping toplevel alive")
                 (log2:info "Threads:~%~{~34T~a~%~}" (bordeaux-threads:all-threads))
                 (sleep 600)))))
      (if start-sentinel
          (sentinel)
          (bordeaux-threads:make-thread #'sentinel)))))

(defvar *races-dir*
  (merge-pathnames (make-pathname :directory '(:relative "races") :type "json")
                   *source-root*)
  "A string designating the directory containing race definitions")

(defvar *races-ht* (make-hash-table :test #'equalp))

(defvar +races-ht-lock+
  (bordeaux-threads:make-lock "races-ht"))

(defun race-info-is-gfs025 (race-info)
  (string= (joref (race-info-data race-info) "gfs025") "TRUE"))

(defun create-routing (&key race-id)
  (let ((race-info (race-info race-id)))
    (typecase race-info
      (race-info-rs
       (let ((resolution
               (if (race-info-is-gfs025 race-info)
                   "0p25"
                   "1p00")))
       (make-routing :race-id race-id
                     :interpolation :bilinear
                     :resolution resolution
                     :merge-start 6d0
                     :merge-window 0d0
                     :options '("realsail"))))
      (race-info-vr
       (make-routing :race-id race-id
                     :interpolation :vr
                     :resolution "1p00"
                     :merge-start 4.0d0
                     :merge-window 1d0
                     :options '("hull" "foil" "winch" "heavy" "light" "reach")))
      (null
       (error "Unknown race-id ~a" race-id)))))

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
  (bordeaux-threads:with-lock-held (+races-ht-lock+)
    (clrhash *races-ht*)
    (loop
      :for name :in (directory (merge-pathnames directory (make-pathname :name :wild :type "json")))
      :do (let ((json-object (parse-json-file name)))
            (cond
              ((typep json-object 'array)
               (store-race-data-vr json-object))
              ((joref json-object "results")
               (store-race-data-rs json-object))
              (T
               (log2:warning "Skipping ~a" name)))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
