;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-11-06 00:23:33>

(in-package :virtualhelm)

;;; Avoid line-breaks when printing log messages
(setf *print-pretty* nil)

(setf (log2:log-level "virtualhelm") log2:+info+)
(setf (log2:log-level "polarcl") log2:+info+)
(setf (log2:log-level "cl-weather") log2:+trace+)
(setf (log2:log-level "mbedtls") log2:+info+)

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
    (load-forecast-bundle 'noaa-bundle)
    ;; Start asynchronous updates
    (timers:add-timer (lambda ()
                        (let ((bundle (get-forecast-bundle 'noaa-bundle)))
                          (update-forecast-bundle bundle)))
                      :hours '(3 9 15 21)
                      :minutes '(30))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defparameter *fc* nil)
(defun get-fc ()
  (setf *fc* (get-forecast (get-forecast-bundle 'noaa-bundle) (now))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
