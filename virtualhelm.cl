;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2022-03-27 16:10:58>

(in-package :virtualhelm)

;;; Avoid line-breaks when printing log messages

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
    (cond
      ((probe-file rcfile)
       (log2:info "Loading ~a " rcfile)        
       (load rcfile :verbose t :print t))
      (t
       (log2:warning "~a does not exist" rcfile)))

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
  ;; Penalty factors:
  ;; 75s @ 50% -> 0.9375 (600s * 0.0625 = 37.5s)
  ;; 300s@ 50% -> 0.75   (600s * 0.25   = 150s)
  ;; 300s@ 95% -> 0.975  (600s * 0.025  = 15s)
  (let ((race-info (race-info race-id)))
    (typecase race-info
      (race-info-rs
       (make-routing :race-id race-id
                     :interpolation :bilinear
                     :resolution (race-info-resolution race-info)
                     :merge-start 6d0
                     :merge-window 0d0
                     :options '("realsail")
                     :penalties (make-penalty :sail 0.975d0 :tack 1d0 :gybe 1d0)))
      (race-info-vr
       (make-routing :race-id race-id
                     :interpolation :vr
                     :resolution "1p00"
                     :merge-start 4.0d0
                     :merge-window 1d0
                     :minwind t
                     :options '("hull" "foil" "winch" "heavy" "light" "reach")
                     :penalties (make-penalty :sail 0.9375d0 :tack 0.9375d0 :gybe 0.9375d0)))
      (null
       (error "Unknown race-id ~a" race-id)))))

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
                ((joref json-object "results")
                 (store-race-data-rs json-object))
                (T
                 (log2:warning "Skipping ~a" name))))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
