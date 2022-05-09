;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2022-03-29 00:32:05>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

(require "asdf")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quicklisp
(load "/opt/src/quicklisp/quicklisp.lisp")
(quicklisp-quickstart:install) 

(ql:quickload "cl-utilities")
(ql:quickload "parse-float")
(ql:quickload "cffi")
(ql:quickload "cl-base64")
(ql:quickload "local-time")
(ql:quickload "zlib")
(ql:quickload "usocket")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load bitsailor & save image

(asdf:load-system "virtualhelm" :force t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization (".vhrc")
;;; CL-MAP
(setf cl-map:*map-file* "/srv/maps/land-polygons-split-4326/land_polygons.shp")
(setf vh:*use-bitmap* nil)
;; (setf cl-map:*bitmap-latpoints* 10800)
;; (setf cl-map:*bitmap-lonpoints* 21600)
;; (setf cl-map:*bitmap-file* "/srv/maps/bm-tiled-10800.dat")

;;; CL-WEATHER
(setf cl-weather:*grib-directory* "/srv/weather/current/")
(setf cl-weather:*use-range-query* t)
(setf cl-weather:*noaa-gfs-path* cl-weather::+NCEP-FTPPRD+)

;;; Router
(setf vh:*api-key* (sb-ext:posix-getenv "BITSAILOR_API_KEY"))

(setf vh:*db* "/etc/bitsailor/main.sdb")

(setf vh:*races-dir* "/etc/bitsailor/races/")
(setf vh:*polars-dir* "/etc/bitsailor/polars/")

(setf vh:*tracks* t)
(setf vh:*twa-steps* 5d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create image

(sb-ext:save-lisp-and-die "bitsailor.core" :toplevel #'vh:run-virtual-helm :executable t) 

(sb-ext:quit)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
