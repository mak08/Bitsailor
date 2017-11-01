;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-11-01 16:03:47>

(defpackage "VIRTUALHELM"
  (:nicknames "VH")
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "POLARCL"
        "CL-ECCODES"
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
