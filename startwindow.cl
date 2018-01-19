;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-19 01:04:31>

(in-package :virtualhelm)

(defun check-window (first last &key
                                  (logfile "startwindow.log")
                                  (start +marseille+)
                                  (dest +carthago+)
                                  (polars "maxi_trimaran_x10percent")
                                  (options  '("winch" "foil" "heavy" "hull")))
  (adjust-timestamp! first (:set :minute 0) (:set :sec 0))
  (adjust-timestamp! last (:set :minute 0) (:set :sec 0))
  (with-open-file (f logfile :direction :output :if-exists :supersede)
    (do ((starttime first
                    (adjust-timestamp starttime (:offset :hour 1)))
         (result (list)))
        ((timestamp> starttime last)
         result)
      (let ((parameters
             (make-routing :start start
                           :dest dest
                           :stepmax (* 3 +6h+)
                           :polars polars
                           :options options
                           :starttime starttime)))
      (multiple-value-bind (route error)
          (ignore-errors
            (get-route parameters))
        (cond
          (route
           (push (routeinfo-stats route) result)
           (format f "~a~%" (routeinfo-stats route)))
          (t
           (format f "Error: ~a, parameters were ~a ~%" error parameters))))))))

#+()
(check-window  (adjust-timestamp (now) (:offset :hour 8))
               (adjust-timestamp (now) (:offset :day 6))
               :options '("winch" "foil" "heavy" "reach" "hull")
               :logfile "d+6r.log")

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
