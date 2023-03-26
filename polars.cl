;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2023-03-26 16:28:25>

(in-package :bitsailor)

(defstruct polars id label name tws twa sails)
(defstruct (polars-vr (:include polars)) winch)
(defstruct (polars-rs (:include polars)))
(defstruct cpolars id label name maxspeed sailspeeds twa vmg speed)
(defstruct sail name speed)

(defmethod print-object ((thing cpolars) stream)
  (format stream "[Compiled polars ~a]" (cpolars-name thing)))
(defmethod print-object ((thing polars) stream)
  (format stream "[Polars ~a]" (polars-name thing)))


(defvar +jib+ 0)
(defvar +spi+ 1)
(defvar +c0rs+ 2)
(defvar +sty+ 2)
(defvar +ljb+ 3)
(defvar +cd0+ 4)
(defvar +hgn+ 5)
(defvar +lgn+ 6)
(defvar +allsails+ 127)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polars preprocessing: Precompute best sail & speed for each wind speed and TWA

(defvar *combined-polars-ht* (make-hash-table :test 'equal))

(defvar *polars-id-ht* (make-hash-table :test 'equalp))
(defvar *polars-name-ht* (make-hash-table :test 'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defun encode-options (option-list)
  (let ((options 3))
    (when (member "light" option-list :test #'string=)
      (setf options (dpb 1 (byte 1 +ljb+) options))
      (setf options (dpb 1 (byte 1 +lgn+) options)))
    (when (member "heavy" option-list :test #'string=)
      (setf options (dpb 1 (byte 1 +sty+) options))
      (setf options (dpb 1 (byte 1 +hgn+) options)))
    (when (member "reach" option-list :test #'string=)
      (setf options (dpb 1 (byte 1 +cd0+) options)))
    (when (member "realsail" option-list :test #'string=)
      (setf options (dpb 1 (byte 1 +c0rs+) options)))
    options))

(defun get-max-speed (cpolars-speed twa wind-speed)
  (declare (double-float twa wind-speed))
  ;; (check-type twa angle)
  (let ((dim (array-dimension cpolars-speed 1)))
    (aref cpolars-speed
          (round (* (abs twa) 10d0))
          (min (round (* wind-speed 10d0))
               (1- dim)))))

(defun get-combined-polars (id &optional (options +allsails+))
  ;; cpolar speeds are in m/s, not kts!
  (log2:info "id=(~a)~a options=~a" (type-of id) id options)
  (let ((polars-ht
         (or (gethash id *combined-polars-ht*)
             (setf (gethash id *combined-polars-ht*)
                   (make-hash-table :test 'eql)))))
    (or (gethash options polars-ht)
        (setf (gethash options polars-ht)
              (preprocess-polars id options)))))

(defun best-vmg (cpolars windspeed)
  (let ((index (round (* windspeed 10d0)))
        (vmg (cpolars-vmg cpolars)))
    (cond
      ((< index (length vmg))
       (values-list (aref vmg index)))
      (T
       (log2:trace "Windpseed ~a exceeds interpolation range 0..~a" windspeed (/ (length vmg) 10d0))
       (values-list (aref vmg (1- (length vmg))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(defun preprocess-polars (id options)
  (let* ((polars (get-polars-by-id id))
         (tws (polars-tws polars))
         (twa (polars-twa polars))
         (max-wind (ceiling (aref tws (1- (length tws)))))
         (twa-steps (if (null *twa-steps*)
                        twa
                        (loop :for s :from 30d0 :to 170d0 :by *twa-steps* :collect s)))
         (maxspeed (make-array (list (1+ 1800)
                                     (1+ (* max-wind 10))))))
    (loop
      :for angle :from 0 :to 1800
      :do (loop
            :for wind :from 0 :to (* max-wind 10)
            :do (setf (aref maxspeed angle wind)
                      (multiple-value-list
                       (get-max-speed% (/ angle  10.d0) (/ wind 10.d0) polars options)))))
    (make-cpolars :id id
                  :label (polars-label polars)
                  :name (polars-name polars)
                  :twa twa-steps
                  :maxspeed maxspeed
                  :vmg (precompute-vmg maxspeed max-wind))))

(defun precompute-vmg (cpolars-speed max-wind)
  (let ((precomputed
         (loop
            :for windspeed :from 0d0 :to max-wind :by 0.1
            :collect (multiple-value-list
                      (best-vmg% windspeed cpolars-speed)))))
    (make-array (length precomputed)
                :initial-contents precomputed)))

(defstruct vmg
  (vmg 0d0 :type double-float)
  (twa 0d0 :type double-float)
  (sail nil :type t))

(defun best-vmg% (windspeed cpolars-speed)
  (loop
     :with best-vmg-up = 0.0d0
     :with best-twa-up = 0.0d0
     :with best-sail-up = nil
     :with best-vmg-down = 0.0d0
     :with best-twa-down = 0.0d0
     :with best-sail-down = nil
     :for angle :from 0.0d0 :to 170.0d0
     :for (speed sail) = (get-max-speed cpolars-speed angle windspeed)
     :for vmg = (* speed (cos (rad angle)))
     :when (< vmg best-vmg-down)
       :do (setf best-twa-down angle
                 best-vmg-down vmg
                 best-sail-down sail)
     :when (> vmg best-vmg-up)
       :do (setf best-twa-up angle
                 best-vmg-up vmg
                 best-sail-up sail)
     :finally (return
                (if (= best-vmg-up best-vmg-down)
                    (values (make-vmg :vmg best-vmg-up :sail best-sail-up :twa 00d0)
                            (make-vmg :vmg (abs best-vmg-down) :sail best-sail-down :twa 180d0))
                    (values (make-vmg :vmg best-vmg-up :sail best-sail-up :twa best-twa-up)
                            (make-vmg :vmg (abs best-vmg-down) :sail best-sail-down :twa best-twa-down))))))

(defun get-max-speed% (angle wind-speed polars options)
  (do
   ((tws (polars-tws polars))
    (twa (polars-twa polars))
    (imax 0)
    (vmax 0)
    (i 0 (1+ i)))
   ((= i 7)
       (values (max vmax 0.01d0)
               (sail-name (aref (polars-sails polars) imax))))
    (when (= (ldb (byte 1 i) options) 1)
      (let ((v (get-boat-speed (abs angle)
                               wind-speed
                               tws
                               twa
                               (sail-speed (aref (polars-sails polars) i)))))
        (when (>= v vmax)
          (setf imax i
                vmax v))))))

(defun get-boat-speed (angle wind-speed tws twa sailspeeds)
  (multiple-value-bind
        (speed-index speed-fraction)
      (fraction-index wind-speed tws)
    (multiple-value-bind
          (angle-index angle-fraction)
        (fraction-index angle twa)
      (bilinear-unit angle-fraction
                     speed-fraction
                     (aref sailspeeds angle-index speed-index)
                     (aref sailspeeds angle-index (1+ speed-index))
                     (aref sailspeeds (1+ angle-index) speed-index)
                     (aref sailspeeds (1+ angle-index) (1+ speed-index))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from file

(defun get-polars-by-name (name)
  (let ((polars (gethash name *polars-name-ht*)))
    (or polars
        (error "No polars found with id=(~a)~a" (type-of name) name))))

(defun get-polars-by-id (id)
  (let ((polars (gethash id *polars-id-ht*)))
    (or polars
        (error "No polars found with id=(~a)~a" (type-of id) id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from JSON
;;; JSON polars are in deg and kts, while GRIB data is in m/s!

(defun load-polars-directory (&key (directory *polars-dir*))
  (loop
     :for name :in (directory (merge-pathnames directory (make-pathname :name :wild :type "json")))
     :do
        ;; Side-effects are performed by the RS/VR functions
        (let ((filename (merge-pathnames name
                                         (merge-pathnames (make-pathname :type "json")
                                                          (pathname *polars-dir*)))))
          (load-polars-file filename))))

(defun load-polars-file (filename)
  (let* ((json-object  (parse-json-file filename)))
    (cond ((joref json-object "scriptData")
           (translate-polars-vr filename json-object))
          ((joref json-object "objectId")
           (translate-polars-rs filename json-object))
          (t
           (error "Unknown polars format")))))

(defun boat-performance (name)
  (let ((polars (get-combined-polars name (encode-options '("foil" "reach" "heavy" "light")))))
    (values
     (round
      (loop
         :for windspeed :from 2d0 :to 25d0 :by 5d0
         :sum (loop :for twa :from 40d0 :to 150d0 :by 5d0
                 :sum (car (get-max-speed (cpolars-speed polars) twa windspeed))))
      10))))


(defun boat-vmg-performance (name)
  (let ((polars (get-combined-polars name (encode-options '("foil" "reach" "heavy" "light")))))
    (values
     (round
      (loop
         :for windspeed :from 2d0 :to 35d0 :by 5d0
         :sum (loop :for vmg :in (multiple-value-list (best-vmg polars windspeed))
                 :sum (car vmg)))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
