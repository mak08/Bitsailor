;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2023
;;; Last Modified <michael 2023-04-01 21:32:37>

(in-package :bitsailor)

;; r-factor: percentual recover of energy (stamina)
(declaim (inline r-factor))
(defun-t r-factor double-float ((tws double-float) (step-size fixnum))
  (let ((duration (cond ((< tws (knots-to-m/s 30d0))
                         (+ 300d0 (* 600d0 (/ tws (knots-to-m/s 30d0)))))
                        (t
                         900d0))))
    (/ step-size duration)))

;; e-factor: percentual reduction of energy (stamina)
(declaim (inline e-factor))
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
      ((string= event "tolerance")
       0d0)
      (t
       (error "not reached")))))

(declaim (inline energy))
(defun-t energy double-float (routepoint reason tws step-size)
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

;; s-factor: maneouvering time factor depending on stamina
;; stamina 0%   -> factor 2
;; stamina 100% -> factor 0.5
;; (declaim (inline s-factor))
(defun s-factor (stamina)
  (- 2d0 (/ stamina 66.667d0)))

(defstruct penalty-spec
  (windspeed-l 0d0 :type double-float)
  (windspeed-h 0d0 :type double-float)
  (factor-l 0d0 :type double-float)
  (factor-h 0d0 :type double-float)
  (time-l 0d0 :type double-float)
  (time-h 0d0 :type double-float))

(defvar *penalty-spec-ht*
  (make-hash-table :test #'equalp))

(defun get-penalty-spec (cpolars event &key (winch-mode "pro"))
  (or (gethash (list cpolars event winch-mode) *penalty-spec-ht*)
      (setf (gethash (list cpolars event winch-mode) *penalty-spec-ht*)
            (let* ((polars (get-polars-by-id (cpolars-id cpolars)))
                   (winch (polars-vr-winch polars))
                   (spec (joref (joref winch event) winch-mode)))
              (when (null spec)
                (error "Missing winch spec, outdated polar file?"))
              (let* ((lws (knots-to-m/s (joref winch "lws")))
                     (hws (knots-to-m/s (joref winch "hws")))
                     (spec-lw (joref spec "lw"))
                   (spec-hw (joref spec "hw")))
                (make-penalty-spec :windspeed-l (coerce lws 'double-float)
                                   :windspeed-h (coerce hws 'double-float)
                                   :factor-l (coerce (joref spec-lw "ratio") 'double-float)
                                   :factor-h (coerce (joref spec-hw "ratio") 'double-float)
                                   :time-l (coerce (joref spec-lw "timer") 'double-float)
                                   :time-h (coerce (joref spec-hw "timer") 'double-float)))))))

(declaim (inline penalty-parameters))
(defun-t penalty-parameters (values double-float double-float) (cpolars event winch-mode tws)
  (let* ((pspec (get-penalty-spec cpolars event :winch-mode winch-mode))
         (lws (penalty-spec-windspeed-l pspec))
         (hws (penalty-spec-windspeed-h pspec))
         (ratio-l (penalty-spec-factor-l pspec))
         (ratio-h (penalty-spec-factor-h pspec))
         (time-l (penalty-spec-time-l pspec))
         (time-h (penalty-spec-time-h pspec))
         (fraction (coerce
                    (cond
                      ((< tws lws)
                       0d0)
                      ((<= lws tws hws)
                       (/ (- tws lws) (- hws lws)))
                      (t
                       1d0))
                    'double-float))
         (factor (linear fraction ratio-l ratio-h))
         (time (linear fraction time-l time-h)))
    (values factor time)))

(declaim (inline step-penalty-factor))
(defun step-penalty-factor (penalty-duration penalty-speed-factor step-duration)
  (/ (+ (* penalty-speed-factor penalty-duration)
        (- step-duration penalty-duration))
     step-duration))

(declaim (inline penalty))
(defun-t  penalty (values double-float double-float) (cpolars event winch-mode tws stepsize energy)
    (multiple-value-bind (factor time)
        (penalty-parameters cpolars event winch-mode tws)
      (let* ((s-factor (s-factor energy))
             (penalty-time  (* time s-factor)))
        (values
         (step-penalty-factor penalty-time factor stepsize)
         penalty-time))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
