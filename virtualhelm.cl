;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-01-06 16:35:32>

(in-package :virtualhelm)

;;; Avoid line-breaks when printing log messages
(setf *print-pretty* nil)

;;; Log levels are also set in server configuration file!
(setf (log2:log-level "virtualhelm") log2:+info+)
(setf (log2:log-level "cl-weather") log2:+trace+)

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
    ;; Load latest complete bundle and possbily update (synchronous)
    (load-dataset 'cl-weather:noaa-dataset)
    ;; Start asynchronous updates
    (timers:add-timer (lambda ()
                        (let ((bundle (get-dataset 'cl-weather:noaa-dataset)))
                          (update-dataset 'noaa-dataset)))
                      :id "WEATHER-UPDATER"
                      :hours '(3 9 15 21)
                      :minutes '(30))))

    
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
