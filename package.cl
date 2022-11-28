;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2022-11-16 21:29:05>

(defpackage BITSAILOR
  (:nicknames "ROUTER")
  (:use "COMMON-LISP"
        "MACROS"
        "PARSE-FLOAT"
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
   ;; Parameters & configuration
   "*RCFILE*"
   "*SERVER-CONFIG*"
   "*API-KEY*"
   "*DB*"
   "*USE-BITMAP*"
   "*RESOLUTIONS*"
   "*WEB-ROOT-DIRECTORY*"
   "*POLARS-DIR*"
   "*RACES-DIR*"
   "*TWA-STEPS*"
   "*RS-MAX-HOURS*"
   "*VR-MAX-HOURS*"
   "*TRACKS*"
   ;; HTTP API
   "GET-PAGE"
   "ACTIVATE-ACCOUNT"
   "signUp"
   "getSession"
   "removeSession"
   "getRaceInfo"
   "getWind"
   "getWindTile"
   "setRoute"
   "setParameter"
   "getRoute"
   "getRouteRS"
   "getRaceList"
   "resetNMEAConnection"
   "getBoatPosition"
   "getTWAPath"
   "checkWindow"
   ;; Main 
   "START-ROUTER"
   ;; Constraints
   "ADD-CONSTRAINT"
   "LIMIT-SOUTH"
   "LIMIT-NORTH"
   "LIMIT-WEST"
   "LIMIT-EAST"
   "EASTBOUND-SOUTH-GATE"
   "EASTBOUND-NORTH-GATE"))

(defpackage VIRTUALHELM
  (:nicknames "VH")
  (:use "COMMON-LISP")
  (:export
   ;; Parameters & configuration
   "getRouteRS"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
