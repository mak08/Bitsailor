;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2022-02-20 22:41:16>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from JSON
;;;
;;; JSON polars are in deg and kts, while GRIB data is in m/s!

(defun translate-polars-rs (polars-name json-object)
  (let ((polars (translate-polars-rs% polars-name json-object)))
    (setf (gethash (polars-id polars) *polars-id-ht*) polars)
    (setf (gethash (polars-name polars) *polars-name-ht*) polars)))

(defun translate-polars-rs% (polars-name json-object)
  ;;; Speed values are in knots. Convert to m/s.
  ;;; Angles are integer deg values. Coerce to double-float because double float is used in simulation
  (let* ((*read-default-float-format* 'double-float)
         (id
           (joref json-object "objectId"))
         (label
           (joref json-object "classBoat"))
         (maxspeed
           nil)
         (polar-data
           (joref json-object "polarData"))
         (jib
           (joref polar-data "jib"))
         (gnk
           (joref polar-data "gennaker"))
         (spi
           (joref polar-data "spi"))
         (tws
           (check-tws jib gnk spi))
         (twa
           (check-twa jib gnk spi))
         (sails
           (make-array 3 :initial-contents (list jib gnk spi)))
         (saildefs
           (make-array (length sails))))
    (log2:info "~20,,a Id ~5,,a, Sails ~a, TWS=~a, TWA=~a" label id (length sails) (length tws) (length twa))
    (setf (aref saildefs 0)
          (make-sail :name "jib"
                     :speed (get-speeddata-rs twa tws jib)))
    (setf (aref saildefs 1)
          (make-sail :name "gennaker"
                     :speed (get-speeddata-rs twa tws gnk)))
    (setf (aref saildefs 2)
          (make-sail :name "spi"
                     :speed (get-speeddata-rs twa tws spi)))
    (loop
      :for speed-index :below (length tws)
      :do (setf (aref tws speed-index)
                (knots-to-m/s (read-from-string (aref tws speed-index)))))
    (loop
      :for angle-index :below (length twa)
      :do (setf (aref twa angle-index)
                (coerce (aref twa angle-index) 'double-float)))
    (make-polars-rs :name polars-name
                    :id id
                    :label label
                    :maxspeed maxspeed
                    :tws tws
                    :twa twa
                    :sails saildefs)))

(defun get-speeddata-rs (twa tws saildef)
  (let ((speeddata (make-array (list (length twa) (length tws))))
        (*read-default-float-format* 'double-float))
    (loop
      :for twa-index :below (length twa)
      :for twa-value :across twa
      :do (loop
            :for tws-index :below (length tws)
            :for tws-value :across tws
            :do (setf (aref speeddata twa-index tws-index)
                      (knots-to-m/s (get-speed-value saildef twa-index tws-value)))))
    speeddata))

(defun get-speed-value (saildef twa-index tws-value)
  ;; Return speed 0 for the artificial 80kn entry
  (read-from-string
   (or (joref (aref saildef twa-index) tws-value)
       "0.0")))

(defun twa-steps (saildef)
  (let ((*read-default-float-format* 'double-float))
    (loop
      :for entry :across saildef
      :collect  (read-from-string (joref entry "twa")))))

(defun check-twa (jib gnk spi)
  (let ((jib-steps (twa-steps jib))
        (gnk-steps (twa-steps gnk))
        (spi-steps (twa-steps spi)))
    (every #'= jib-steps gnk-steps spi-steps)
    (make-array (length jib-steps) :initial-contents jib-steps)))

(defun check-tws (jib gnk spi)
  (let* ((all-tws
           (append
            (map 'list (lambda (e) (map 'list #'json-field-name (json-object-fields e))) jib)
            (map 'list (lambda (e) (map 'list #'json-field-name (json-object-fields e))) gnk)
            (map 'list (lambda (e) (map 'list #'json-field-name (json-object-fields e))) spi)))
         (tws-values
           ;; No actual checking, assume all TWS steps are the same, use the first one.
           (map 'vector
                (lambda (e) e)
                (cdr (first all-tws)))))
    ;; Add an artificial 80kn entry
    (concatenate 'vector tws-values #("80.00"))))

(defun max-speed (jib gnk spi)
  (format t "~{~{~a~^;~}~%~}~%"
          (loop
            :for j :across jib
            :for g :across gnk
            :for s :across spi
            :collect (loop
                       :for f :in (json-object-fields j)
                       :collect (max (read-from-string
                                      (joref j (json-field-name f)))
                                     (read-from-string
                                      (joref g (json-field-name f)))
                                     (read-from-string
                                      (joref s (json-field-name f))))))))
                   

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
