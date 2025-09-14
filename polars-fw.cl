;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2025-09-14 16:28:29>

(in-package :bitsailor)

(defun translate-polars-fw (polars-name json-object)
  (let* ((polars (translate-polars-fw% polars-name json-object))
         (id (polars-id polars))
         (name (polars-name polars)))
    (setf (gethash name *polars-name-ht*) polars)
    (setf (gethash id *polars-id-ht*) polars)))

(defun translate-polars-fw% (polars-name json-object)
  ;;; Speed values are in knots. Convert to m/s.
  ;;; Angles are integer deg values. Coerce to double-float because double float is used in simulation
  (let* ((polars
           (joref json-object "data_json"))
         (id
           (joref json-object "id"))
         (label
          (joref json-object "name"))
         (tws
          (joref polars "tws"))
         (twa
          (joref polars "twa"))
         (sails
           (make-array 1 :initial-element (joref polars "table")))
         (saildefs
           (make-array (length sails))))
    (log2:info "~30,,a Id ~5,,a, Sails ~a, TWS=~a, TWA=~a" label id (length sails) tws twa)
    (loop
       :for saildef :across sails
       :for k :from 0
       :do (let ((speeddata (make-array (list (length twa) (length tws)))))
             (loop
                :for angle-index :below (length twa)
                :do (loop
                       :for speed-index :below (length tws)
                       :do (setf (aref speeddata angle-index speed-index)
                                 (knots-to-m/s (aref (aref  saildef angle-index) speed-index)))))
             (setf (aref saildefs k)
                   (make-sail :name "UniSail"
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
    (make-polars-fw :name polars-name
                    :id label
                    :label label
                    :tws tws
                    :twa twa
                    :sails saildefs)))
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
