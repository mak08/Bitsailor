;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-08-30 22:51:26>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "POLARCL"
        "CL-GRIB2"
        "CFFI"
        "LOCAL-TIME"
        "BABEL")
  (:export "getWind"
           "setRoute"
           "getRoute"
           "getWindAtPosition"
           "START-GRIB-UPDATES"
           "FORMAT-TIMESPEC-DATEHH"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
