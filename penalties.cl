;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2023
;;; Last Modified <michael 2023-03-06 23:05:41>

(in-package :bitsailor)

;; (declaim (inline energy))
(defun energy (routepoint reason tws step-size)
  (cond ((null reason)
         ;; recover
         (let ((recover
                 (r-factor tws step-size))) 
           (min 100d0
                (+ (routepoint-energy routepoint) recover))))
        (t
         (let ((decrease
                 (e-factor tws reason)))
           (max 0d0
                (- (routepoint-energy routepoint) decrease))))))

;; r-factor: percentual recover of energy (stamina)
;; (declaim (inline r-factor))
(defun-t r-factor double-float ((tws double-float) (step-size fixnum))
  (let ((duration (cond ((< tws (knots-to-m/s 30d0))
                         (+ 300d0 (* 600d0 (/ tws (knots-to-m/s 30d0)))))
                        (t
                         900d0))))
    (/ step-size duration)))

;; e-factor: percentual reduction of energy (stamina)
;; (declaim (inline e-factor))
(defun e-factor (tws event)
  (let ((factor (cond ((< tws (knots-to-m/s 30d0))
                       (1+ (/ tws (knots-to-m/s 30d0))))
                      (t
                       2d0))))
    (cond
      ((or (string= event "tack")
           (string= event "gybe"))
       (* factor 10d0))
      ((string= event "sailChange")
       (* factor 20d0))
      (t
       (error "not reached")))))

;; s-factor: maneouvering time factor depending on stamina
;; stamina 0%   -> factor 2
;; stamina 100% -> factor 0.5
;; (declaim (inline s-factor))
(defun s-factor (stamina)
  (- 2d0 (/ stamina 66.667d0)))

;; (declaim (inline penalty-parameters))
(defun penalty-parameters (cpolars event tws)
  (let* ((polars (get-polars-by-id (cpolars-id cpolars)))
         (winch (polars-vr-winch polars))
         (lws (knots-to-m/s (joref winch "lws")))
         (hws (knots-to-m/s (joref winch "hws")))
         (spec-pro (joref (joref winch event) "pro"))
         (spec-pro-lw (joref spec-pro "lw"))
         (spec-pro-hw (joref spec-pro "hw"))
         (ratio-l (coerce (joref spec-pro-lw "ratio") 'double-float))
         (ratio-h (coerce (joref spec-pro-hw "ratio") 'double-float))
         (time-l (coerce (joref spec-pro-lw "timer") 'double-float))
         (time-h (coerce (joref spec-pro-hw "timer") 'double-float))
         ;; ToDo: cache the PRO timer values to avoid double coercion each time
         (fraction (coerce
                    (cond
                      ((< tws lws)
                       0)
                      ((<= lws tws hws)
                       (/ (- tws lws) (- hws lws)))
                      (t
                       1))
                    'double-float))
         (factor (linear fraction ratio-l ratio-h))
         (time (linear fraction time-l time-h)))
    (values factor time)))

(defun step-penalty-factor (penalty-duration penalty-speed-factor step-duration)
  (/ (+ (* penalty-speed-factor penalty-duration)
        (- step-duration penalty-duration))
     step-duration))

(defun penalty (cpolars event tws stepsize stamina)
  (multiple-value-bind (factor time)
      (penalty-parameters cpolars event tws)
    (values
     (step-penalty-factor (* time (s-factor stamina)) factor stepsize)
     time)))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
