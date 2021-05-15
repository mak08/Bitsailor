;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2021-05-12 01:04:36>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
        "MACROS"
        #+sbcl "SB-MOP"
        #+:ccl "CCL"
        "CL-GEOMATH"
        "POLARCL"
        "CL-MAP"
        "CL-WEATHER"
        "LOCAL-TIME"
        "RDPARSE"
        #-ccl"BABEL")
  (:export
   "*DB*"
   "*SERVER-CONFIG*"
   "*POLARS-DIR*"
   "*TWA-STEPS*"
   "*RACES-DIR*"
   "*REACHED-DISTANCE*"
   ;; HTTP API
   "GET-PAGE"
   "ACTIVATE-ACCOUNT"
   "signUp"
   "getSession"
   "getLegInfo"
   "getWind"
   "getWindForecast"
   "probeWind"
   "setRoute"
   "setParameter"
   "getRoute"
   "getRaceList"
   "resetNMEAConnection"
   "getBoatPosition"
   "getTWAPath"
   "checkWindow"
   ;; VH backend
   "RUN-VIRTUAL-HELM"
   ;; Constraints
   "ADD-CONSTRAINT"
   "LIMIT-SOUTH"
   "LIMIT-NORTH"
   "LIMIT-WEST"
   "LIMIT-EAST"
   "EASTBOUND-SOUTH-GATE"
   "EASTBOUND-NORTH-GATE"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
