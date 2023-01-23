;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2023-01-24 00:26:29>

(in-package :bitsailor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun distance-knots (lat0 lon0 lat1 lon1 seconds)
  (m/s-to-knots
   (/ (course-distance (make-latlng :lat lat0 :lng lon0)
                       (make-latlng :lat lat1 :lng lon1))
      seconds)))

(defun compare-speed (polars-name twa tws &optional (options '("reach" "heavy" "light")))
  (let* ((cpolars (get-combined-polars polars-name (encode-options options)))
         (polars (get-polars-by-name polars-name))
         (polars-raw (get-polars-raw polars-name))
         (twa-index (position twa (joref polars-raw "twa") :test #'equalp))
         (tws-index (position tws (joref polars-raw "tws") :test #'equalp)))
    (destructuring-bind (speed sail)
        (get-max-speed (cpolars-speed cpolars) twa (knots-to-m/s tws))
      (format t "Compiled:         ~a~%"
              (m/s-to-knots speed))
      (format t "Raw interpolated: ~a~%"
              (loop
                 :for saildef :across  (joref polars-raw "sail")
                 :when (equal (joref saildef "name") sail)
                 :return (get-boat-speed-raw twa tws (joref polars-raw "twa") (joref polars-raw "tws") saildef)))
      (format t "Converted:        ~a~%"
              (m/s-to-knots (get-max-speed% twa (knots-to-m/s tws) polars (encode-options options))))
      (format t "Raw:              ~a~%"
              (when (and twa-index tws-index)
                (loop
                   :for saildef :across  (joref polars-raw "sail")
                   :when (equal (joref saildef "name") sail)
                   :return (aref (aref (joref saildef "speed") twa-index) tws-index))))
      (format t "Sail:             ~a~%"
              sail))))

(defun get-boat-speed-raw (twa tws twa-values tws-values saildef)
  (multiple-value-bind
        (speed-index speed-fraction)
      (fraction-index tws tws-values)
    (multiple-value-bind
          (angle-index angle-fraction)
        (fraction-index twa twa-values)
      (bilinear-unit speed-fraction
                     angle-fraction
                     (aref (aref (joref saildef "speed") angle-index) speed-index)
                     (aref (aref (joref saildef "speed") angle-index) (1+ speed-index))
                     (aref (aref (joref saildef "speed") (1+ angle-index)) speed-index)
                     (aref (aref (joref saildef "speed") (1+ angle-index)) (1+ speed-index))))))

(defun boat-speed-kn (polars-name twa tws &optional (options '("reach" "heavy" "light")))
  (let ((cpolars (get-combined-polars polars-name (encode-options options))))
    (destructuring-bind (speed sail)
        (get-max-speed (cpolars-speed cpolars) twa (knots-to-m/s tws))
      (values (m/s-to-knots speed)
              sail))))

(defun boat-speed-raw (polars-name twa tws &optional (options '("reach" "heavy" "light")))
  (declare (ignorable options))
  (let* ((polars (get-polars-raw polars-name))
         (twa-index (position twa (joref polars "twa") :test #'equalp))
         (tws-index (position tws (joref polars "tws") :test #'equalp))
         (saildefs
          (joref polars "sail")))
    (loop
       :for saildef :across saildefs
       :collect (list (joref saildef "name")
                      (aref (aref (joref saildef "speed") twa-index) tws-index)))))

(defvar *polars-raw-ht* (make-hash-table :test #'equal))
(defun get-polars-raw (polars-name)
  (or (gethash polars-name *polars-raw-ht*)
      (setf (gethash polars-name *polars-raw-ht*)
            (joref (joref (parse-json-file polars-name) "scriptData") "polar"))))

(defun export-routing-isochrone-csv (filename routing number)
  (let ((isochrones (joref routing "isochrones")))
    (assert (< number (length isochrones)))
    (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (write-isochrone-csv f (aref isochrones number)))))

(defun write-isochrone-csv (stream isochrone)
  (format stream "~a;~a~%" "lat" "lon")
  (loop
    :for p :across (joref isochrone "path")
    :do  (format stream "~,4F;~,4F~%" (joref p "lat") (joref p "lng"))))


(defun export-routing-gpx (filename routing)
  (let ((isochrones (joref routing "isochrones")))
    (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (format f "~a~%" "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>")
      (format f "~a~%" "<gpx>")
      (loop
        :for iso :across isochrones
        :do (write-isochrone-gpx f iso))
      (format f "~a~%" "</gpx>"))))

(defun write-isochrone-gpx (stream isochrone)
  (format stream "~a~%" "<trk>")
  (format stream "~a~%" "<trkseg>")
  (loop
    :for p :across (joref isochrone "path")
    :do  (format stream "<trkpt lat=\"~,4F\" lon=\"~,4F\"></trkpt>~%"
                 (joref p "lat")
                 (tweak-long (joref p "lng"))))
  (format stream "~a~%" "</trkseg>")
  (format stream "~a~%" "</trk>"))

(defun tweak-long (x)
  (if (> x 180d0) (- x 360d0)
      (if (< x -180d0) (+ x 360d0)
          x)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
