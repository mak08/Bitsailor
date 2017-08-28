;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2017-08-24 20:30:19>

(defsystem "virtualhelm"
  :description "Sailing route optimization using isochrones search"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-grib2" "polarcl" "log2" "cffi" "cl-utilities")
  :serial t
  :components ((:file "package")
               (:file "libraries")
               (:file "macros")
               (:file "datatypes")
               (:file "geomath")
               (:file "gdal-api")
               (:file "meteodata")
               (:file "polars")
               (:file "map")
               (:file "meteodata-dwd")
               (:file "simulation")
               (:file "json")
               (:file "http-api")
               (:file "test")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

