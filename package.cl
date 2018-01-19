;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-01-19 01:06:20>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "POLARCL"
        "CL-MAP"
        "CL-WEATHER"
        "LOCAL-TIME"
        "RDPARSE"
        "BABEL")
  (:export
   ;; HTTP API
   "GET-PAGE"
   "getSession"
   "getWind"
   "setRoute"
   "setParameter"
   "getRoute"
   "getTWAPath"
   "checkWindow"
   ;; VH backend
   "*MAP-FILE*"
   "*GRIB-FOLDER*"
   "RUN-VIRTUAL-HELM"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
