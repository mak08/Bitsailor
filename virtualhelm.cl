;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2021-05-21 18:11:14>

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

    
    ;; The GSHHS coastline is abysmally slow
    ;; (ensure-map :filename "/home/michael/Maps/GSHHS/GSHHS_shp/h/GSHHS_h_L1.shp")
    (ensure-map)
    (ensure-bitmap)
    
    (load-race-definitions-rs)
    (load-all-polars-rs)
    (log2:info "Loading server configuration ~a" *server-config*)
    (polarcl:load-configuration *server-config*)
    ;; Load latest complete bundle and possbily update (synchronous), start asynchronous updates.
    (bordeaux-threads:make-thread (lambda () (noaa-start-updates)) :name "INITIAL-WEATHER-UPDATE")))

(defvar *races-ht* (make-hash-table :test #'equalp))
(defvar *races-dir*
  (merge-pathnames (make-pathname :directory '(:relative "races") :type "json")
                   *source-root*)
  "A string designating the directory containing polar files")

(defvar +races-ht-lock+
  (bordeaux-threads:make-lock "races-ht"))

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

(defvar *races-dir-rs*
  (merge-pathnames (make-pathname :directory '(:relative "races-rs") :type "json")
                   *source-root*)
  "A string designating the directory containing polar files")

(defun load-race-definitions-rs ()
  (bordeaux-threads:with-lock-held (+races-ht-lock+)
    (clrhash *races-ht*)
    (let* ((filename (merge-pathnames *races-dir-rs* (make-pathname :name "Races" :type "json")))
           (races (parse-json-file filename)))
      (loop
        :for race :across (joref races "results")
        :do (store-leg-data-rs race)))))

;;; Used for testing
(defun load-race-definition (name)
  (let* ((path (merge-pathnames *races-dir* (make-pathname :name name :type "json")))
         (race-def  (parse-json-file name)))
    (get-leg-data race-def)))
    
;; caution - get-leg-info also exists
(defun get-leg-data (json-object)
  (cond
    ((joref json-object "scriptData")
     (let* ((leg (joref (joref json-object "scriptData") "leg"))
            (race-id (joref (joref leg  "_id") "race_id"))
            (leg-num (joref (joref leg "_id") "num"))
            (leg-id (format nil "~a.~a" race-id leg-num)))
       (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
       (setf (race-info leg-id) leg)))
    ((joref json-object "res")
     (let* ((leg (joref (joref json-object "res") "leg"))
            (race-id (joref (joref leg  "_id") "race_id"))
            (leg-num (joref (joref leg "_id") "num"))
            (leg-id (format nil "~a.~a" race-id leg-num)))
       (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
       (setf (race-info leg-id) leg)))
    (T
     (error "Unexpected JSON format"))))

(defun store-leg-data-rs (race-def)
  (let* ((race-id (joref race-def "objectId"))
         (race-name (joref race-def "name")))
    (log2:info "Loading race ~a ~a" race-id race-name)
    (setf (gethash race-id *races-ht*) race-def)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
