;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2021-10-18 20:10:52>

(defsystem "virtualhelm"
  :description "Sailing route optimization using isochrones search"
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "parse-float" "makros" "cl-geomath" "rdparse" "polarcl" "cl-map" "cl-weather" "cl-rdbms")
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "macros")
               (:file "database")
               (:file "datatypes")
               (:file "polars")
               (:file "polars-rs")
               (:file "polars-vr")
               (:file "races-vr")
               (:file "races-rs")
               (:file "constraints")
               (:file "filter")
               (:file "simulation")
               (:file "json")
               (:file "nmea")
               (:file "http-api")
               (:file "signup")
               (:file "virtualhelm")
               (:file "race-constraints")
               (:file "test")
               (:file "util")
               (:file "startwindow")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
