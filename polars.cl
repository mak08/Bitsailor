;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-12-04 20:49:30>

(in-package :virtualhelm)

(defvar *polars-dir*
  (append (pathname-directory (asdf:system-source-directory :virtualhelm)) '("polars")))

(defparameter +sail-names+
    #("Jib" "Spi" "Staysail" "Light Jib" "Code 0" "Heavy Gennaker" "Light Gennaker"))

(defparameter +polar-file-names+
  #("vpp_1_1.csv" "vpp_1_2.csv" "vpp_1_4.csv" "vpp_1_8.csv" "vpp_1_16.csv" "vpp_1_32.csv" "vpp_1_64.csv"))

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
      ((imax 0)
       (vmax 0)
       (i 0 (1+ i)))
      ((= i 7)
       (values vmax
               (sail-name (aref polars imax))))
    (when (= (ldb (byte 1 i) options) 1)
      (let ((v (get-boat-speed (polars-angle angle)
                               wind-speed
                               (sail-speed (aref polars i)))))
        (when (>= v vmax)
          (setf imax i
                vmax v))))))

(defun get-boat-speed (angle wind-speed polars)
  (let* ((w-max-index (- (array-dimension polars 1) 1))
         (w-max (aref polars 0 w-max-index))
         (a-max-index (- (array-dimension polars 0) 1))
         (a-max 180)
         (w1 (cond
               ((> wind-speed w-max)
                (error "Cannot interpolate wind beyond ~a" w-max))
               (t
                (loop
                   :for k :from 1 :to (1- w-max-index)
                   :while (<=  (aref polars 0 k) wind-speed)
                   :finally (return k)))))
         (a1 (loop
                :for k :from 1 :to (1- a-max-index)
                :while (<= (aref polars k 0) angle)
                :finally (return k)))
         (w0 (if (= wind-speed w-max) w1 (1- w1)))
         (a0 (if (= angle a-max) a1 (1- a1))))
    (when (>= w1 w-max) (decf w1))
    (when (>= a1 a-max) (decf a1))  
    (bilinear wind-speed angle
              (aref polars 0 w0)
              (aref polars 0 w1)
              (aref polars a0 0)
              (aref polars a1 0)
              (aref polars a0 w0)
              (aref polars a0 w1)
              (aref polars a1 w0)
              (aref polars a1 w1))))

(defun polars-angle (heading)
  (let ((angle (if (> heading 180)
                   (- heading 360)
                   heading)))
    (abs angle)))

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
         (first-sail (aref polars 0))
         (first-sail-speed (sail-speed first-sail))
         (wind-values (array-dimension first-sail-speed 1))
         (max-wind (aref first-sail-speed 0 (1- wind-values)))
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
  (let ((polars-dir (merge-pathnames (make-pathname :directory (list :relative polars))
                                     (make-pathname :directory *polars-dir*))))
    (cond
      ((probe-file polars-dir)
       (log2:info "Loading polars from ~a~%" polars-dir)
       (map 'vector
            (lambda (filename sailname)
              (make-sail :name sailname
                         :speed (parse-polars
                                 (load-polars (merge-pathnames (make-pathname :name filename)
                                                               polars-dir)))))
            +polar-file-names+
            +sail-names+))
      (t
       (let ((polars-file (merge-pathnames (make-pathname :name polars :type "json")
                                           (make-pathname :directory *polars-dir*))))
         (log2:info "Loading polars from ~a~%" polars-file)
         (let ((sails (load-polars-json :filename polars-file)))
           (make-array (length sails) :initial-contents sails)))))))

(defun load-polars (polars-filename)
  (cond
    ((null (probe-file polars-filename))
     (error "Missing file ~a" polars-filename))
    (t
     (with-open-file (f polars-filename :element-type '(unsigned-byte 8))
       (let ((octets (make-array (file-length f) :element-type  '(unsigned-byte 8))))
         (read-sequence octets f)
         octets)))))


(defun parse-polars (octets)
  (let ((*read-default-float-format* 'double-float))
    (let* ((string (octets-to-string octets))
           (values
            (mapcar (lambda (s)
                      (mapcar (lambda (x) (unless (string= x "TWA")
                                            (coerce (read-from-string x) 'double-float)))
                              (cl-utilities:split-sequence #\; s)))
                    (cl-utilities:split-sequence #\newline string))))
      (make-array (list (length values)
                        (length (car values)))
                  :initial-contents values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading polars from JSON
;;;
;;; JSON polars are in deg and kts!


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
              (progn
                (log2:info "Reading sail data")
                (loop
                   :for saildef :across (joref polar "sail")
                   :collect (let ((speeddata (make-array (list (1+ (length twa)) (1+ (length tws))))))
                              (loop
                                 :for speed :across tws
                                 :for s :from 0
                                 :do (setf (aref speeddata 0 (1+ s))
                                           (if convert-speed
                                               (knots-to-m/s (aref tws s))
                                               (aref tws s))))
                              (loop
                                 :for angle :across twa
                                 :for a :from 0
                                 :do (setf (aref speeddata (1+ a) 0)
                                           (if convert-speed
                                               (coerce (aref twa a) 'double-float)
                                               (aref twa a)))
                                 :do (loop
                                        :for speed :across tws
                                        :for s :from 0
                                        :do (setf (aref speeddata (1+ a) (1+ s))
                                                  (if convert-speed
                                                      (knots-to-m/s (aref (aref (joref saildef "speed") a) s))
                                                      (aref (aref (joref saildef "speed") a) s)))))
                              (make-sail :name (joref saildef "name")
                                         :speed speeddata))))))
        saildefs))))


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
