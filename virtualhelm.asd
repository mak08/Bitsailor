;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2017-11-01 16:03:59>

(defsystem "virtualhelm"
  :description "Sailing route optimization using isochrones search"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-eccodes" "polarcl" "log2" "cffi" "rdparse" "cl-utilities")
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "macros")
               (:file "datatypes")
               (:file "gribfile")
               (:file "geomath")
               (:file "gdal-api")
               (:file "meteodata")
               (:file "polars")
               (:file "map")
               (:file "meteodata-dwd")
               (:file "meteodata-noaa")
               (:file "forecast-access")
               (:file "simulation")
               (:file "json")
               (:file "places")
               (:file "http-api")
               (:file "virtualhelm")
               (:file "util")
               (:file "test")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

