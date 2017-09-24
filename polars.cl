;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-09-24 01:51:47>

(in-package :virtualhelm)

(defvar *polars-dir*
  (append (pathname-directory (asdf:system-source-directory :virtualhelm)) '("polars")))

(defparameter +sail-names+
    #("Jib" "Gennaker" "Jib 2" "Jib 3" "Code 0" "Heavy Gennaker" "Light Gennaker"))

(defparameter +polar-file-names+
  #("vpp_1_1.csv" "vpp_1_2.csv" "vpp_1_4.csv" "vpp_1_8.csv" "vpp_1_16.csv" "vpp_1_32.csv" "vpp_1_64.csv"))

(defun get-max-speed (angle wind-speed polars-name &key (fast t))
  ;; angle: difference between boat heading and wind direction
  (if fast
      (let ((polars (get-combined-polars polars-name)))
        (values-list (aref polars
                           (round (polars-angle angle) 0.1)
                           (round wind-speed 0.1))))
      (let ((polars (get-polars polars-name)))
        (get-max-speed% angle wind-speed polars))))

(defun get-max-speed% (angle wind-speed polars)
  (loop
     :with imax = 0
     :with vmax = 0
     :for i :below 7
     :for v = (get-boat-speed (polars-angle angle)
                              wind-speed
                              (sail-speed (aref polars i)))
     :do (when (>= v vmax)
           (setf imax i
                 vmax v))
     :finally (return
                (values vmax
                        (sail-name (aref polars imax))))))

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
                   :for c = (aref polars 0 k)
                   :while (<=  c wind-speed)
                   :finally (return k)))))
         (a1 (loop
                :for k :from 1 :to (1- a-max-index)
                :for r = (aref polars k 0)
                :while (<=  r angle)
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

(defun get-combined-polars (name)
  (or (gethash name *combined-polars-ht*)
      (setf (gethash name *combined-polars-ht*)
            (preprocess-polars name))))

(defun preprocess-polars (name)
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
                                   (get-max-speed% angle wind polars))))))
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

(defstruct sail name speed)

(defun load-polars-json (&key (filename  "/home/michael/Repository/VirtualHelm/polars/IMOCA60VVOR17.json"))
  (with-open-file (f filename :element-type 'character)
    (let ((json-string (make-string (file-length f))))
      (read-sequence json-string f)
      (let* ((polar (joref (joref (parse-json json-string) "scriptData") "polar"))
             (tws (joref polar "tws"))
             (twa (joref polar "twa"))
             (saildefs (joref polar "sail")))
        (log2:info "Reading sail data")
        (loop
           :for saildef :across saildefs
           :collect (let ((speeddata (make-array (list (1+ (length twa)) (1+ (length tws))))))
                      (loop
                         :for speed :across tws
                         :for s :from 0
                         :do (setf (aref speeddata 0 (1+ s))
                                   (coerce (aref tws s) 'double-float)))
                      (loop
                         :for angle :across twa
                         :for a :from 0
                         :do (setf (aref speeddata (1+ a) 0) (coerce (aref twa a) 'double-float))
                         :do (loop
                                :for speed :across tws
                                :for s :from 0
                                :do (setf (aref speeddata (1+ a) (1+ s))
                                          (coerce (aref (aref (joref saildef "speed") a) s) 'double-float))))
                      (make-sail :name (joref saildef "name")
                                 :speed speeddata)))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
