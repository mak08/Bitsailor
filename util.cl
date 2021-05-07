;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2021-05-06 23:27:33>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun distance-knots (lat0 lon0 lat1 lon1 seconds)
  (m/s-to-knots
   (/ (course-distance (make-latlng :lat lat0 :lng lon0)
                       (make-latlng :lat lat1 :lng lon1))
      seconds)))


(defparameter *fc* nil)
(defun get-fc ()
  (setf *fc* (get-forecast (get-dataset 'noaa-dataset) (now))))

(defun get-route-for-session (session-id race-id)
  (let ((session (gethash session-id *session-ht*)))
    (unless session
      (error "~a: no such session" session-id)) 
    (get-route (session-routing session race-id))))


(defun compare-speed (polars-name twa tws &optional (options '("reach" "heavy" "light")))
  (let* ((cpolars (get-combined-polars polars-name (encode-options options)))
         (polars (get-polars polars-name))
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
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
