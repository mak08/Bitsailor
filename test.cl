;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-05 23:09:48>

(in-package :virtualhelm)

(defun test-getWind ()
  (let ((request (polarcl:make-http-get))
        (response (polarcl::make-http-response))
        (location nil))
    (|getWind| location request response :|north| "55.0" :|south| "45.0" :|west| "5.0" :|east| "15.0" :|ddx| "0.2")
    (http-body response)))

#|
(COURSE-ANGLE
      #S(LATLNG
         :LAT 54.434403d0
         :LNG 11.361632d0)
      #S(LATLNG
         :LAT 54.44400124404393d0
         :LNG 11.361632d0))
|#

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(- 11.361632d0 11.361631409815484d0)
