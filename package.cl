;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-08-24 20:22:50>

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
           "getWindAtPosition"
           "START-GRIB-UPDATES"
           "FORMAT-TIMESPEC-DATEHH"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
