;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-10-16 22:59:39>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "POLARCL"
        "CL-GRIB2"
        "CFFI"
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
   ;; VH backend
   "*MAP-FILE*"
   "*GRIB-FOLDER*"
   "RUN-VIRTUAL-HELM"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
