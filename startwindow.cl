;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2024-02-29 18:45:25>

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
                       (logfile "startwindow")
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
    (do ((starttime first
                    (adjust-timestamp starttime (:offset :minute increment)))
         (result (list)))
        ((timestamp> starttime last)
         result)
      (let* ((parameters
               (get-routing-presets "VR"
                                    :race-id race-id
                                    :slat slat
                                    :slon slon
                                    :dlat dlat
                                    :dlon dlon
                                    :stepmax stepmax
                                    :options options
                                    :sail "Jib"
                                    :starttime starttime))
             (path
               (merge-pathnames (format nil "~a-~a" logfile (routing-cycle parameters))
                                (make-pathname :type "log"))))

        (with-open-file (f path
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
          (multiple-value-bind (route error)
              (ignore-errors
               (get-route parameters))
            (cond
              (route
               (push (routeinfo-stats route) result)
               (format f "~a ~a ~a~%"
                       (routing-cycle parameters)
                       (routestats-start (routeinfo-stats route))
                       (routestats-duration  (routeinfo-stats route))))
              (t
               (format f "Error: ~a, parameters were ~a ~%" error parameters)))))))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
