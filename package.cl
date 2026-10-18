;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2026-01-09 13:47:45>

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
   "*DB*"
   "*USE-BITMAP*"
   "*WEB-ROOT-DIRECTORY*"
   "*POLARS-DIR*"
   "*RACES-DIR*"
   "*TWA-STEPS*"
   "*MAX-ANGLE*"
   "*MAX-ROUTE-HOURS*"
   "*MAX-ISO-POINTS*"
   "*TRACKS*"
   ;; HTTP API
   "ACTIVATE-ACCOUNT"
   "signUp"
   "getSession"
   "getServerSettings"
   "removeSession"
   "getRoute"
   "getBoatPosition"
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
