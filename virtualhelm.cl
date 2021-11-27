;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2021-11-27 16:13:15>

(in-package :virtualhelm)

;;; Avoid line-breaks when printing log messages
(setf *print-pretty* nil)

;;; Log levels are also set in server configuration file!
(setf (log2:log-level "virtualhelm") log2:+info+)
(setf (log2:log-level "cl-weather") log2:+info+)

(log2:info "Heap space: ~a" (sb-ext:dynamic-space-size))

(defvar *server-config* 
  (merge-pathnames (make-pathname :name "server-config" :type "cl")
                   *source-root*))

(defun run-virtual-helm (&optional (start-sentinel t))
  (log2:info "Path: ~a " #.*compile-file-truename*)
  (let ((rcfile
         (merge-pathnames (make-pathname :name ".vhrc")
                          (user-homedir-pathname))))
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

    ;; Load latest complete bundle and possbily update (synchronous), start asynchronous updates.
    (log2:info "Starting weather updates")
    (bordeaux-threads:make-thread (lambda ()
                                    (download-cycle (previous-cycle (available-cycle (now))))
                                    (noaa-start-updates))
                                  :name "INIT-WEATHER-DATA")
    
    ;; Start web server
    (log2:info "Starting web server ~a" *server-config*)
    (polarcl:load-configuration *server-config*)

    ;; Keep function alive, this is our toplevel
    (when start-sentinel
      (loop (progn (sleep 600) 
                   (log2:info "Sentinel takes a look around"))))))

(defvar *races-dir*
  (merge-pathnames (make-pathname :directory '(:relative "races") :type "json")
                   *source-root*)
  "A string designating the directory containing race definitions")

(defvar *races-ht* (make-hash-table :test #'equalp))

(defvar +races-ht-lock+
  (bordeaux-threads:make-lock "races-ht"))

(defun create-routing (&key race-id)
  (let ((race-info (race-info race-id)))
    (typecase race-info
      (race-info-rs
       (make-routing :race-id race-id
                     :interpolation :bilinear
                     :merge-start 6d0
                     :merge-window 0d0
                     :options '("realsail")))
      (race-info-vr
       (make-routing :race-id race-id
                     :interpolation :vr
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
