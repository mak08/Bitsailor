;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-10-08 20:52:41>

(in-package :virtualhelm)

(setf (log2:log-level "virtualhelm") log2:+info+)
(setf (log2:log-level "polarcl") log2:+info+)
(setf (log2:log-level "mbedtls") log2:+warning+)

(defvar *server-config* 
  (merge-pathnames (make-pathname :name "server-config" :type "cl")
                   (make-pathname :directory (pathname-directory #.*compile-file-truename*)))) 

(defun run-virtual-helm ()
  (log2:info "Path: ~a " #.*compile-file-truename*)
  (let ((rcfile
         (merge-pathnames (make-pathname :name ".vhrc")
                          (user-homedir-pathname))))
    (when (probe-file rcfile)
      (log2:info "Loading ~a " rcfile)        
      (load rcfile :verbose t :print t))
    (ensure-map)
    (log2:info "Loading server configuration ~a" *server-config*)
    (polarcl:load-configuration *server-config*)
    (start-grib-updates)))

(defun start-grib-updates (&key (bundles '(noaa-bundle)) (resolution "0.250"))
  "Check for new forecast data an remove old data periodically"
  (flet ((run-updates ()
           (loop
              (dolist (bundle bundles)
                (handler-case
                    (update-forecast-bundle bundle :resolution resolution)
                  (error (e)
                    (log2:error "~a" e))))
              (sleep 3600))))
    (bordeaux-threads:make-thread #'run-updates :name "FORECASTS-UPDATER")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
