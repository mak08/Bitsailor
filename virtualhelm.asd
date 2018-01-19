;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2018-01-18 22:55:50>

(defsystem "virtualhelm"
  :description "Sailing route optimization using isochrones search"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-utilities" "log2" "rdparse" "cl-map" "cl-weather" "polarcl")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "datatypes")
               (:file "geomath")
               (:file "polars")
               (:file "simulation")
               (:file "filter")
               (:file "json")
               (:file "places")
               (:file "http-api")
               (:file "virtualhelm")
               (:file "util")
               (:file "test")
               (:file "startwindow")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
