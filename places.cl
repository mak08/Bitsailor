;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-10-30 22:48:57>

(in-package :virtualhelm)

(defun find-place (name)
  (let ((symbol
         (find-symbol (format nil "+~@:(~a~)+" name) :virtualhelm)))
    (if symbol
        (symbol-value symbol)
        (make-latlng :lat 0d0 :lng 0d0))))

(defvar +LACORUNA+
  (make-latlng :lat 43.484812d0 :lng -8.589764d0))

(defvar +LESSABLES+
  (make-latlng :lat 46.479120d0 :lng -1.794153d0))

(defvar +FEHMARN+
  (make-latlng :lat 54.434403d0 :lng 11.361632d0))

(defvar +YSTAD+
  (make-latlng :lat 55.391123d0 :lng 13.792635d0))

(defvar +ALICANTE+
  (make-latlng :lat 38.322222d0 :lng  -0.48444d0))

(defvar +LISBON+
  (make-latlng :lat 38.561906d0 :lng -9.379930d0))

(defvar +GIBRALTAR+
  (make-latlng :lat 35.961877d0 :lng -5.570280d0))

(defvar +PORTOSANTO+
  (make-latlng :lat 33.005019d0 :lng -16.42808d0))

;; Point Nemo
;; 48°52.6′S 123°23.6′W

(defvar +capetown+
  (make-latlng :lat -33.91028d0 :lng 18.300278d0))

(defvar +freemantle+
  (make-latlng :lat -32.060833d0 :lng 115.72361d0))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
