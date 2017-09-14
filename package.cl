;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-14 22:06:34>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "POLARCL"
        "CL-GRIB2"
        "CFFI"
        "LOCAL-TIME"
        "BABEL")
  (:export
   ;; HTTP API
   "getWind"
   "setRoute"
   "setParameter"
   "getRoute"
   "getWindAtPosition"
   ;; VH backend
   "*MAP-FILE*"
   "*GRIB-FOLDER*"
   "START-GRIB-UPDATES"
   "ENSURE-MAP"
   "IS-LAND"
   "FORMAT-TIMESPEC-DATEHH"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
