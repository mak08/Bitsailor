;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2021-10-29 21:58:32>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from JSON
;;;
;;; JSON polars are in deg and kts, while GRIB data is in m/s!

(defun translate-polars-vr (polars-name json-object)
  (let* ((polars (translate-polars-vr% polars-name json-object))
         (id (polars-id polars))
         (name (polars-name polars)))
    (setf (gethash name *polars-name-ht*) polars)
    (setf (gethash id *polars-id-ht*) polars)))

(defun normalize-vr-polar-id (id)
  (multiple-value-bind (intpart digits)
      (truncate  id)
    (unless (zerop digits)
      (error "Invalid ID ~a" id))
    (format nil "~a" intpart)))

(defun translate-polars-vr% (polars-name json-object)
  ;;; Speed values are in knots. Convert to m/s.
  ;;; Angles are integer deg values. Coerce to double-float because double float is used in simulation
  (let* ((polars
           (joref (joref json-object "scriptData") "polar"))
         (id
           (normalize-vr-polar-id (joref polars "_id")))
         (label
          (joref polars "label"))
         (maxspeed
          (joref polars "maxSpeed"))
         (tws
          (joref polars "tws"))
         (twa
          (joref polars "twa"))
         (sail
          (joref polars "sail"))
         (saildefs
           (make-array (length sail))))
    (log2:info "~30,,a Id ~5,,a, Sails ~a, TWS=~a, TWA=~a" (joref polars "label") id (length sail) tws twa)
    (loop
       :for saildef :across sail
       :for k :from 0
       :do (let ((speeddata (make-array (list (length twa) (length tws)))))
             (loop
                :for angle-index :below (length twa)
                :do (loop
                       :for speed-index :below (length tws)
                       :do (setf (aref speeddata angle-index speed-index)
                                 (knots-to-m/s (aref (aref (joref saildef "speed") angle-index) speed-index)))))
             (setf (aref saildefs k)
                   (make-sail :name (joref saildef "name")
                              :speed speeddata))))
    ;; Convert TWS values to m/s
    (loop
       :for speed-index :below (length tws)
       :do (setf (aref tws speed-index)
                 (knots-to-m/s (aref tws speed-index))))
    (loop
       :for angle-index :below (length twa)
       :do (setf (aref twa angle-index)
                 (coerce (aref twa angle-index) 'double-float)))
    (make-polars-vr :name polars-name
                    :id (format nil "~a" id)
                    :label label
                    :maxspeed maxspeed
                    :tws tws
                    :twa twa
                    :sails saildefs)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
