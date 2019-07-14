;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-07-05 21:09:25>

(in-package :virtualhelm)

;;; Avoid line-breaks when printing log messages
(setf *print-pretty* nil)

;;; Log levels are also set in server configuration file!
(setf (log2:log-level "virtualhelm") log2:+info+)
(setf (log2:log-level "cl-weather") log2:+info+)

(defvar *server-config* 
  (merge-pathnames (make-pathname :name "server-config" :type "cl")
                   *source-root*))

(defun run-virtual-helm ()
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
    (ensure-map)
    (log2:info "Loading server configuration ~a" *server-config*)
    (polarcl:load-configuration *server-config*)
    ;; Load latest complete bundle and possbily update (synchronous), start asynchronous updates.
    (noaa-start-updates)))

    
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
