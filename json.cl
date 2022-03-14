;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;;   A naïve function to convert Lisp data to JSON format.
;;; Author        Michael Kappert 2014
;;; Last Modified  <michael 2022-02-25 23:48:34>
 
(in-package :virtualhelm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special types

(defmethod json% (stream (thing duration))
  (format stream "\"~s\"" thing))


(defmethod json% (stream (thing vector))
  (typecase thing
    (latlng
     (format stream "{\"lat\":~5$, \"lng\":~5$}"
             (latlng-lat thing)
             (latlng-lng thing)))
    (t
     (format stream "[")
     (loop
        :for k :below (length thing)
        :for element :across thing
        :do (json% stream element)
        :when (< k (1- (length thing))) :do (format stream ", "))
     (format stream "]"))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
