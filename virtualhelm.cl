;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2020-10-24 21:36:57>

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
    (load-race-definitions)
    (load-all-polars)
    (log2:info "Loading server configuration ~a" *server-config*)
    (polarcl:load-configuration *server-config*)
    ;; Load latest complete bundle and possbily update (synchronous), start asynchronous updates.
    (bordeaux-threads:make-thread (lambda () (noaa-start-updates)) :name "INITIAL-WEATHER-UPDATE")))

(defvar *races-ht* (make-hash-table :test #'equalp))
(defvar *races-dir*
  (merge-pathnames (make-pathname :directory '(:relative "races") :type "json")
                   *source-root*)
  "A string designating the directory containing polar files")

(defun load-race-definitions ()
  (loop
     :for name :in (directory (merge-pathnames *races-dir* (make-pathname :name :wild :type "json")))
     :do (let ((race-def (parse-json-file name)))
           (get-leg-data race-def))))

(defun get-leg-data (json-object)
  (cond
    ((joref json-object "scriptData")
     (let* ((leg (joref (joref json-object "scriptData") "leg"))
            (race-id (joref (joref leg  "_id") "race_id"))
            (leg-num (joref (joref leg "_id") "num"))
            (leg-id (format nil "~a.~a" race-id leg-num)))
       (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
       (setf (gethash leg-id *races-ht*) leg)))
    ((joref json-object "res")
     (let* ((leg (joref (joref json-object "res") "leg"))
            (race-id (joref (joref leg  "_id") "race_id"))
            (leg-num (joref (joref leg "_id") "num"))
            (leg-id (format nil "~a.~a" race-id leg-num)))
       (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
       (setf (gethash leg-id *races-ht*) leg)))))      
    
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
