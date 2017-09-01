;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-09-01 21:56:59>

(in-package :virtualhelm)

(defvar *polars-dir*
  (append (pathname-directory (asdf:system-source-directory :virtualhelm)) '("polars")))

(defparameter +sail-names+
    #("Jib" "Gennaker" "Jib 2" "Jib 3" "Code 0" "Heavy Gennaker" "Light Gennaker"))

(defparameter +polar-file-names+
  #("vpp_1_1.csv" "vpp_1_2.csv" "vpp_1_4.csv" "vpp_1_8.csv" "vpp_1_16.csv" "vpp_1_32.csv" "vpp_1_64.csv"))


(defun fetch-polars (polars)
  (let ((polars-dir (merge-pathnames (make-pathname :directory (list :relative polars))
                                     (make-pathname :directory *polars-dir*))))
    (log2:info "Loading polars from ~a~%" polars-dir)
    (map 'vector
         (lambda (filename)
           (parse-polars
            (load-polars (merge-pathnames (make-pathname :name filename)
                                          polars-dir))))
         +polar-file-names+)))

(defun load-polars (polars-filename)
  (cond
    ((null (probe-file polars-filename))
     (error "Missing file ~a" polars-filename))
    (t
     (with-open-file (f polars-filename :element-type '(unsigned-byte 8))
       (let ((octets (make-array (file-length f) :element-type  '(unsigned-byte 8))))
         (read-sequence octets f)
         octets)))))

(defun get-max-speed (angle wind-speed)
  ;; angle: difference between boat heading and wind direction
  (loop
     :with imax = 0
     :with vmax = 0
     :for i :below 7
     :for v = (get-boat-speed (polars-angle angle)
                              wind-speed
                              (aref *polars* i))
     :do (when (>= v vmax)
           (setf imax i
                 vmax v))
     :finally (return
                (values vmax
                        (aref +sail-names+ imax)))))

(defun get-boat-speed (angle wind-speed polars)
  (assert (<= 0 angle 180))
  (let ((w (loop
              :for k :from 1
              :for c = (aref polars 0 k)
              :while (<=  c wind-speed)
              :finally (return k)))
        (a (loop
              :for k :from 1
              :for r = (aref polars k 0)
              :while (<=  r angle)
              :finally (return k))))
    (bilinear wind-speed angle
              (aref polars 0 (1- w))
              (aref polars 0 w)
              (aref polars (1- a) 0)
              (aref polars a 0)
              (aref polars (1- a) (1- w))
              (aref polars (1- a) w)
              (aref polars a (1- w))
              (aref polars a w))))

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


(defparameter *polars*
  (fetch-polars "VOR14"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
