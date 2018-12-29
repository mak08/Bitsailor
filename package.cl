;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-12-28 11:59:59>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CL-GEOMATH"
        "POLARCL"
        "CL-MAP"
        "CL-WEATHER"
        "LOCAL-TIME"
        "RDPARSE"
        "BABEL")
  (:export
   "*SERVER-CONFIG*"
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
   "RUN-VIRTUAL-HELM"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
