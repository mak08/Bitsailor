;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2019-08-29 22:40:41>

(declaim (optimize (speed 3) (debug 0) (space 1) (safety 1)))

(in-package :virtualhelm)

(defstruct cpolars name twa vmg speed)
(defstruct polars name tws twa sails)
(defstruct sail name speed)

(defmethod print-object ((thing cpolars) stream)
  (format stream "[Compiled polars ~a]" (cpolars-name thing)))
(defmethod print-object ((thing polars) stream)
  (format stream "[Polars ~a]" (polars-name thing)))

(defvar *polars-dir*
  (merge-pathnames (make-pathname :directory '(:relative "polars"))
                   *source-root*)
  "A string designating the directory containing polar files")

(defvar +jib+ 0)
(defvar +spi+ 1)
(defvar +sty+ 2)
(defvar +ljb+ 3)
(defvar +cd0+ 4)
(defvar +hgn+ 5)
(defvar +lgn+ 6)
(defvar +allsails+ 127)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polars preprocessing: Precompute best sail & speed for each wind speed and TWA

(defvar *combined-polars-ht* (make-hash-table :test 'equal))

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
    options))

(defun get-max-speed (cpolars-speed twa wind-speed)
  (declare (double-float twa wind-speed))
  ;; (check-type twa angle)
  (let ((dim (array-dimension cpolars-speed 1)))
    (aref cpolars-speed
          (round (* (abs twa) 10d0))
          (min (round (* wind-speed 10d0))
               (1- dim)))))

(defun get-combined-polars (name &optional (options +allsails+))
  ;; cpolar speeds are in m/s, not kts!
  (let ((polars-ht
         (or (gethash name *combined-polars-ht*)
             (setf (gethash name *combined-polars-ht*)
                   (make-hash-table :test 'eql)))))
    (or (gethash options polars-ht)
        (setf (gethash options polars-ht)
              (preprocess-polars name options)))))

(defun best-vmg (cpolars windspeed)
  (let ((index (round (* windspeed 10d0)))
        (vmg (cpolars-vmg cpolars)))
    (cond
      ((< index (length vmg))
       (values-list (aref vmg index)))
      (T
       (log2:warning "Windpseed ~a exceeds interpolation range 0..~a" windspeed (/ (length vmg) 10d0))
       (values-list (aref vmg (1- (length vmg))))))))

(defun get-cpolars-vmg (name twa kts)
  (let* ((cpolars (get-combined-polars name +allsails+))
         (wind-speed (knots-to-m/s kts))
         (boat-speed (car
                      (get-max-speed (cpolars-speed cpolars) twa wind-speed))))
    (* boat-speed (cos (rad twa)))))

(defun get-polars-vmg (name twa kts)
  (let* ((polars (get-polars name))
         (wind-speed (knots-to-m/s kts))
         (boat-speed (get-max-speed% twa wind-speed polars +allsails+)))
    (* boat-speed (cos (rad twa)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(defun preprocess-polars (name options)
  (let* ((polars (get-polars name))
         (tws (polars-tws polars))
         (twa (polars-twa polars))
         (max-wind (aref tws (1- (length tws))))
         (precomputed
          (loop
             :for angle :from 0 :to 1800
             :collect (loop
                         :for wind :from 0 :to (* max-wind 10)
                         :collect (multiple-value-list
                                   (get-max-speed% (/ angle  10.0) (/ wind 10.0) polars options)))))
         (speed (make-array (list (length precomputed)
                                  (length (car precomputed)))
                            :initial-contents precomputed)))
    (make-cpolars :name name
                  :twa twa
                  :speed speed
                  :vmg (precompute-vmg speed max-wind))))

(defun precompute-vmg (cpolars-speed max-wind)
  (let ((precomputed
         (loop
            :for windspeed :from 0d0 :to max-wind :by 0.1
            :collect (multiple-value-list
                      (best-vmg% windspeed cpolars-speed)))))
    (make-array (length precomputed)
                :initial-contents precomputed)))

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
     :finally (return (values (list best-vmg-up best-sail-up best-twa-up)
                              (list (abs best-vmg-down) best-sail-down best-twa-down)))))

(defun get-max-speed% (angle wind-speed polars options)
  (do
   ((tws (polars-tws polars))
    (twa (polars-twa polars))
    (imax 0)
    (vmax 0)
    (i 0 (1+ i)))
   ((= i 7)
       (values vmax
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

(defvar *polars-ht* (make-hash-table :test 'equal))

(defun get-polars (name)
  (or (gethash name *polars-ht*)
      (setf (gethash name *polars-ht*)
            (load-polars name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from JSON
;;;
;;; JSON polars are in deg and kts, while GRIB data is in m/s!


(defun load-polars (polars-name)
  ;;; Speed values are in knots. Convert to m/s.
  ;;; Angles are integer deg values. Coerce to double-float because double float is used in simulation
  (let* ((polars
          (joref (joref (parse-json-file polars-name) "scriptData") "polar"))
         (tws
          (joref polars "tws"))
         (twa
          (joref polars "twa"))
         (sail
          (joref polars "sail"))
         (saildefs
          (make-array (length sail))))
    (log2:info "Reading ~a sail definitions for TWS=~a TWA=~a" (length sail) tws twa)
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
    (make-polars :name polars-name
                 :tws tws
                 :twa twa
                 :sails saildefs)))

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
