;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2024-06-24 22:20:50>

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
   "*MAX-ANGLE*"
   "*MAX-ROUTE-HOURS*"
   "*MAX-ISO-POINTS*"
   "*VALID-VMG-ANGLES*"
   "*PENALTY-MODE-VR*"
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

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
