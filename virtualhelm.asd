;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2020-07-16 23:42:41>

(defsystem "virtualhelm"
  :description "Sailing route optimization using isochrones search"
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "cl-geomath" "rdparse" "polarcl" "cl-map" "cl-weather" "cl-rdbms")
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "macros")
               (:file "database")
               (:file "datatypes")
               (:file "polars")
               (:file "constraints")
               (:file "simulation")
               (:file "filter")
               (:file "json")
               (:file "http-api")
               (:file "virtualhelm")
               (:file "race-constraints")
               (:file "test")
               (:file "util")
               (:file "startwindow")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
