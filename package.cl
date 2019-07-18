;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-07-18 21:47:06>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
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
   "*SERVER-CONFIG*"
   "*POLARS-DIR*"
   ;; HTTP API
   "GET-PAGE"
   "getSession"
   "getWind"
   "getWindForecast"
   "probeWind"
   "setRoute"
   "setParameter"
   "getRoute"
   "getTWAPath"
   "checkWindow"
   ;; VH backend
   "RUN-VIRTUAL-HELM"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
