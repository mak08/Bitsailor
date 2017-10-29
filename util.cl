;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-10-29 22:46:18>

(in-package :virtualhelm)


(defun polars-json-to-csv (&key
                             (filename  "/home/michael/Repository/VirtualHelm/polars/IMOCA60VVOR17.json"))
  (let* ((saildefs
          (load-polars-json :filename filename :convert-speed nil))
         (pathname (pathname filename))
         (output-directory (append (pathname-directory pathname)
                                   (list (pathname-name pathname))))
         (output-path (make-pathname :directory output-directory)))
    (log2:info "Writing files to ~a" output-path)
    (ensure-directories-exist output-path)
    (loop
       :for saildef :in saildefs
       :do (with-open-file (f (make-pathname :directory output-directory
                                             :name  (sail-name saildef)
                                             :type "csv")
                              :direction :output
                              :if-exists :supersede)
             (write-polars-csv saildef f)))))

(defun write-polars-csv (saildef file)
  (destructuring-bind (twa-length speed-length)
      (array-dimensions (sail-speed saildef))
    (loop
       :for twa :below twa-length
       :do (progn
             (loop
                :for speed :below speed-length
                :do (format file "~a," (aref (sail-speed saildef) twa speed)))
             (format file "~%")))))

(defun probe-wind (time lat lng)
  (multiple-value-bind (angle speed)
      (get-wind-forecast
       (get-forecast
        (get-forecast-bundle 'noaa-bundle)
        (parse-rfc3339-timestring time))
       (make-latlng :lat (coerce lat 'double-float)
                    :lng (coerce lng 'double-float)))
    (values angle
            (m/s-to-knots speed))))
       


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
