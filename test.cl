;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-08-24 20:10:09>

(in-package :virtualhelm)

(defun test-getWind ()
  (let ((request (polarcl:make-http-get))
        (response (polarcl::make-http-response))
        (location nil))
    (|getWind| location request response :|north| "55.0" :|south| "45.0" :|west| "5.0" :|east| "15.0" :|ddx| "0.2")
    (http-body response)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
