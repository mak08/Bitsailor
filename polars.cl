;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-12-10 21:12:17>

(in-package :virtualhelm)

(defvar *polars-dir*
  (append (pathname-directory (asdf:system-source-directory :virtualhelm)) '("polars")))

(defvar +jib+ 0)
(defvar +spi+ 1)
(defvar +sty+ 2)
(defvar +ljb+ 3)
(defvar +cd0+ 4)
(defvar +hgn+ 5)
(defvar +lgn+ 6)
(defvar +allsails+ 127)

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

(defun get-max-speed (angle wind-speed polars-name &key (fast t) (options +allsails+))
  ;; angle: difference between boat heading and wind direction
  (if fast
      (let ((polars (get-combined-polars polars-name options)))
        (values-list (aref polars
                           (round (polars-angle angle) 0.1)
                           (round wind-speed 0.1))))
      (let ((polars (get-polars polars-name)))
        (get-max-speed% angle wind-speed polars options))))

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
      (let ((v (get-boat-speed (polars-angle angle)
                               wind-speed
                               tws
                               twa
                               (sail-speed (aref (polars-sails polars) i)))))
        (when (>= v vmax)
          (setf imax i
                vmax v))))))

(defun polars-angle (heading)
  (let ((angle (if (> heading 180)
                   (- heading 360)
                   heading)))
    (abs angle)))

(defun get-boat-speed (angle wind-speed tws twa sailspeeds)
  (multiple-value-bind
        (speed-index speed-fraction)
      (fraction-index wind-speed tws)
    (multiple-value-bind
          (angle-index angle-fraction)
        (fraction-index angle twa)
      (bilinear-unit speed-fraction
                     angle-fraction
                     (aref sailspeeds angle-index speed-index)
                     (aref sailspeeds (1+ angle-index) speed-index)
                     (aref sailspeeds angle-index (1+ speed-index))
                     (aref sailspeeds (1+ angle-index) (1+ speed-index))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polars preprocessing: Precompute best sail & speed for each wind speed and TWA

(defvar *combined-polars-ht* (make-hash-table :test 'equal))

(defun get-combined-polars (name options)
  (let ((polars-ht
         (or (gethash name *combined-polars-ht*)
             (setf (gethash name *combined-polars-ht*)
                   (make-hash-table :test 'eql)))))
    (or (gethash options polars-ht)
        (setf (gethash options polars-ht)
              (preprocess-polars name options)))))

(defun preprocess-polars (name options)
  (log2:info "Preprocessing polars for ~a/~a" name options)
  (let* ((polars (get-polars name))
         (tws (polars-tws polars))
         (twa (polars-twa polars))
         (max-wind (aref tws (1- (length tws))))
         (precomputed
          (loop
             :for angle :of-type double-float :from 0d0 :to 180d0 :by 0.1d0
             :collect (loop
                         :for wind :from 0d0 :to (- max-wind 0.1d0) :by 0.1d0
                         :collect (multiple-value-list
                                   (get-max-speed% angle wind polars options))))))
    (make-array (list (length precomputed)
                      (length (car precomputed)))
                :initial-contents precomputed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from file

(defvar *polars-ht* (make-hash-table :test 'equal))

(defun get-polars (name)
  (or (gethash name *polars-ht*)
      (setf (gethash name *polars-ht*)
            (fetch-polars name))))

(defun fetch-polars (polars)
  (let ((polars-file (merge-pathnames (make-pathname :name polars :type "json")
                                      (make-pathname :directory *polars-dir*))))
    (log2:info "Loading polars from ~a~%" polars-file)
    (load-polars-json :filename polars-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from JSON
;;;
;;; JSON polars are in deg and kts!

(defstruct polars tws twa sails)
(defstruct sail name speed)

(defun load-polars-json (&key
                           (filename  "/home/michael/Repository/VirtualHelm/polars/IMOCA60VVOR17.json")
                           (convert-speed t))
  ;;; Speed values are on knots. Convert to m/s.
  ;;; Angles are integer deg values. Coerce to double-float because double float is used in simulation
  (with-open-file (f filename :element-type 'character)
    (let ((json-string (make-string (file-length f))))
      (read-sequence json-string f)
      (log2:info "Parsing json")
      (let* ((polar
              (joref (joref (parse-json json-string) "scriptData") "polar"))
             (tws
              (joref polar "tws"))
             (twa
              (joref polar "twa"))
             (saildefs
              (make-array (length (joref polar "sail")))))
        (log2:info "Reading sail data")
        (loop
           :for saildef :across (joref polar "sail")
           :for k :from 0
           :do (let ((speeddata (make-array (list (length twa) (length tws)))))
                 (loop
                    :for angle :across twa
                    :for a :from 0
                    :do (loop
                           :for speed :across tws
                           :for s :from 0
                           :do (setf (aref speeddata a s)
                                     (if convert-speed
                                         (knots-to-m/s (aref (aref (joref saildef "speed") a) s))
                                         (aref (aref (joref saildef "speed") a) s)))))
                 (setf (aref saildefs k)
                       (make-sail :name (joref saildef "name")
                                  :speed speeddata))))
        (when convert-speed
          (loop
             :for speed :across tws
             :for s :from 0
             :do (setf (aref tws s)
                       (knots-to-m/s (aref tws s)))))
        (loop
           :for angle :across twa
           :for a :from 0
           :do (setf (aref twa a)
                     (coerce (aref twa a) 'double-float)))
        (make-polars :tws tws
                     :twa twa
                     :sails saildefs)))))

(defun find-vmg-angles (polars)
  (loop
     :for saildef :in polars
     :collect
     (destructuring-bind (twa-len tws-len)
         (array-dimensions (sail-speed saildef))
       (loop
          :for speed :below tws-len
          :collect (loop
                      :with best-speed = 0
                      :with best-angle = 0
                      :for angle :below twa-len
                      :when (> (* (aref (sail-speed saildef) angle speed) (cos angle))
                               best-speed)
                      :do (setf best-angle angle)
                      :finally (return best-angle))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
