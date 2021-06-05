;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2021-06-01 21:49:08>

(in-package :virtualhelm)

(defvar *polars-dir-rs*
  (merge-pathnames (make-pathname :directory '(:relative "polars") :type "json")
                   *source-root*)
  "A string designating the directory containing polar files")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ToDo: Overwriting original encode-options
(defvar +rs-allsails+ 7)
(defun encode-options (option-list)
  (declare (ignore option-list))
  +rs-allsails+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(defun load-all-polars-rs ()
  (loop
     :for name :in (directory (merge-pathnames *polars-dir-rs* (make-pathname :name :wild :type "json")))
     :do (let ((polars
                (load-polars-from-file name)))
           (setf (gethash (polars-id polars) *polars-id-ht*) polars)
           (setf (gethash (polars-name polars) *polars-name-ht*) polars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from file

(defun get-polars-by-name (name)
  (gethash name *polars-name-ht*))

(defun get-polars-by-id (id)
  ;; (assert (numberp id))
  (gethash id *polars-id-ht*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from JSON
;;;
;;; JSON polars are in deg and kts, while GRIB data is in m/s!

(defun load-polars-from-file (polars-name)
  ;;; Speed values are in knots. Convert to m/s.
  ;;; Angles are integer deg values. Coerce to double-float because double float is used in simulation
  (let* ((*read-default-float-format* 'double-float)
         (polar
           (joref (aref (joref (parse-json-file polars-name) "results") 0) "polar"))
         (id
           (joref polar "objectId"))
         (label
           (joref polar "classBoat"))
         (maxspeed
           nil)
         (polar-data
           (joref polar "polarData"))
         (jib
           (joref polar-data "jib"))
         (gnk
           (joref polar-data "gennaker"))
         (spi
           (joref polar-data "spi"))
         (tws
           (check-tws jib gnk spi))
         (twa
           (check-twa jib gnk spi)
           )
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
    (make-polars :name polars-name
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
                      (knots-to-m/s (read-from-string (joref (aref saildef twa-index) tws-value))))))
    speeddata))
  

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
  (let ((all-tws
          (append
           (map 'list (lambda (e) (map 'list #'json-field-name (json-object-fields e))) jib)
           (map 'list (lambda (e) (map 'list #'json-field-name (json-object-fields e))) gnk)
           (map 'list (lambda (e) (map 'list #'json-field-name (json-object-fields e))) spi))))
    (map 'vector
         (lambda (e) e)
         (cdr (first all-tws)))))

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
