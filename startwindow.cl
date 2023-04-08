;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2023-04-06 20:06:21>

(in-package :bitsailor)

#|
(check-window 12 168
              :increment 180
              :start cl-geomath::+los-angeles+
              :dest cl-geomath::+honolulu+
              :stepmax (* 60 60 24 5)
              :polars "maxi_trimaran")
|#

(defun check-window (race-id
                     start-offset
                     window-size
                     &key
                       (logfile "startwindow.log")
                       (increment 60)
                       (slat 0d0)
                       (slon 0d0)
                       (dlat 0d0)
                       (dlon 0d0)
                       (stepmax (* 60 60 24 6))
                       (polars "11")
                       (options  '("winch" "reach" "foil" "heavy" "light" "hull")))
  (let*
      ((first (adjust-timestamp (now) (:offset :hour start-offset)))
       (last (adjust-timestamp first (:offset :hour window-size))))
    (adjust-timestamp! first (:set :minute 0) (:set :sec 0))
    (adjust-timestamp! last (:set :minute 0) (:set :sec 0))
    (with-open-file (f logfile
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (do ((starttime first
                      (adjust-timestamp starttime (:offset :minute increment)))
           (result (list)))
          ((timestamp> starttime last)
           result)
        (let ((parameters
               (get-routing-presets "VR"
                                    :race-id race-id
                                    :slat slat
                                    :slon slon
                                    :dlat dlat
                                    :dlon dlon
                                    :stepmax stepmax
                                    :options options
                                    :sail "Jib"
                                    :starttime starttime)))
          (multiple-value-bind (route error)
              (ignore-errors
                (get-route parameters))
            (cond
              (route
               (push (routeinfo-stats route) result)
               (format f "~a ~a~%" (routeinfo-stats route) (routeinfo-polars route)))
              (t
               (format f "Error: ~a, parameters were ~a ~%" error parameters)))))))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
