;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-14 23:33:39>

(in-package :virtualhelm)

(defun run-virtual-helm ()
  (log2:info "Path: ~a " #.*compile-file-truename*)
  (let ((polarcl-config
         (merge-pathnames (make-pathname :name "server-config" :type "cl")
                          (make-pathname :directory (pathname-directory #.*compile-file-truename*))))
        (rcfile
         (merge-pathnames (make-pathname :name ".vhrc")
                          (user-homedir-pathname))))
    (when (probe-file rcfile)
      (log2:info "Loading ~a " rcfile)        
      (load rcfile :verbose t :print t))
    (ensure-map)
    (polarcl:load-configuration polarcl-config)
    (start-grib-updates)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
