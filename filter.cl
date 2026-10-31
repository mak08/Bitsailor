;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2026-03-29 21:49:40>

(in-package :bitsailor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gates
;;;
;;; The isochrone alone cannot tell whether a gate was crossed: the frontier keeps
;;; only the point with the largest origin distance per bucket, so the crossing may
;;; hit an interior point of the gate which is not on the frontier. Instead, every
;;; candidate step (predecessor -> new position) is tested against the gate
;;; segment while the isochrone is expanded. That is a handful of flops per candidate.

(defvar *gate-crossings* nil
  "Routepoints on the gate line found while expanding the current isochrone")

(defun gate-crossing (gate p0 p1)
  "Fraction (0..1) of the step P0->P1 at which the gate segment is crossed, or NIL.
Uses a local flat projection around P0, which is accurate for steps and gates of
a few hundred km."
  (let* ((lat0 (latlng-lat p0))
         (lng0 (latlng-lng p0))
         (k (cos (rad lat0))))
    (flet ((x (p) (* k (normalize-angle (- (latlng-lng p) lng0))))
           (y (p) (- (latlng-lat p) lat0)))
      (let* ((rx (x p1)) (ry (y p1))
             (xa (x (gate-a gate))) (ya (y (gate-a gate)))
             (sx (- (x (gate-b gate)) xa))
             (sy (- (y (gate-b gate)) ya))
             (denom (- (* rx sy) (* ry sx))))
        (when (> (abs denom) 1d-12)
          (let ((fraction (/ (- (* xa sy) (* ya sx)) denom))
                (u (/ (- (* xa ry) (* ya rx)) denom)))
            (when (and (<= 0d0 fraction 1d0) (<= 0d0 u 1d0))
              fraction)))))))

;;; Interpolate a crossing position and fractional timestamp, link it to PREDECESSOR and push it
;;; onto *GATE-CROSSINGS*.
(defun record-gate-crossing (gate fraction predecessor new-pos step-size
                             twa heading speed sail wind-dir wind-speed curnt-dir curnt-speed
                             wave-height wave-dir wave-factor)
  (let* ((p0 (routepoint-position predecessor))
         (lat0 (latlng-lat p0))
         (lng0 (latlng-lng p0))
         (pos (make-latlng
               :lat (+ lat0 (* fraction (- (latlng-lat new-pos) lat0)))
               :lng (normalize-angle
                     (+ lng0 (* fraction (normalize-angle (- (latlng-lng new-pos) lng0))))))))
    (push (create-routepoint predecessor pos
                             (adjust-timestamp (routepoint-time predecessor)
                               (:offset :nsec (round (* fraction step-size 1000000000))))
                             twa heading
                             (course-distance pos (gate-next gate))
                             speed sail wind-dir wind-speed curnt-dir curnt-speed 0d0 0d0
                             wave-height wave-dir wave-factor)
          *gate-crossings*)))

(defun best-gate-crossing (&optional (crossings *gate-crossings*))
  "The crossing closest to the next target that does not run over land."
  (find-if-not (lambda (rp)
                 (if *use-bitmap*
                     (bm-is-land (routepoint-position rp))
                     (intersects-land-p (routepoint-position (routepoint-predecessor rp))
                                        (routepoint-position rp))))
               (sort (copy-list crossings) #'<
                     :key #'routepoint-destination-distance)))

(defvar *routepoint-sink* nil
  "Optional candidate consumer used by the shared-clock gate search.")

(declaim (inline bucket))
;;; Convert an origin bearing to an integer angular bucket using DELTA-ANGLE degrees per bucket.
(defun-t bucket fixnum ((origin-angle double-float) (delta-angle double-float))
  (truncate origin-angle delta-angle))
  
(declaim (inline add-routepoint))
;;; Send a candidate to the shared gate-search sink, or retain outward progress in the single-leg
;;; angular bucket and history arrays.
(defun-t add-routepoint null ((predecessor routepoint)
                              (position latlng)
                              (origin-distance double-float)
                              (origin-angle double-float)
                              (delta-angle double-float)
                              (left double-float)
                              (destination-distance double-float)
                              step-size
                              step-time
                              twa
                              heading
                              speed
                              sail
                              wind-dir
                              wind-speed
                              curnt-dir
                              curnt-speed
                              wave-height
                              wave-dir
                              wave-factor)
  (declare (special next-isochrone max-dist min-angle))
  (when *routepoint-sink*
    (funcall *routepoint-sink*
             (create-routepoint predecessor position step-time twa heading
                                destination-distance speed sail wind-dir wind-speed
                                curnt-dir curnt-speed origin-angle origin-distance
                                wave-height wave-dir wave-factor))
    (return-from add-routepoint (values)))
  (let*
      ((maxpoints (length next-isochrone))
       (offset (bucket left delta-angle))
       (bucket (bucket origin-angle delta-angle))
       (max-bucket (bucket origin-angle min-angle)))
    (when
        (or (< bucket 0)
            (>= bucket maxpoints))
      (error "Invalid bucket"))
    (decf bucket offset)
    (when (< bucket 0)
      (incf bucket maxpoints))
    (when (or (null (aref next-isochrone bucket))
              (> origin-distance
                 (routepoint-origin-distance (aref next-isochrone bucket))))
      (when (> origin-distance (aref max-dist max-bucket))
        (setf (aref max-dist max-bucket) origin-distance)
        (setf (aref next-isochrone bucket)
              (create-routepoint predecessor
                                 position
                                 step-time
                                 twa
                                 heading
                                 destination-distance
                                 speed
                                 sail
                                 wind-dir
                                 wind-speed
                                 curnt-dir
                                 curnt-speed
                                 origin-angle
                                 origin-distance
                                 wave-height
                                 wave-dir
                                 wave-factor)))))
  (values))

;;; Remove empty, zoned and land-blocked candidates. Bitmap mode checks endpoints; vector mode
;;; checks segments. LIMITS is unused.
(defun-t filter-isochrone vector ((isochrone vector) &key (limits nil) (zones nil) (use-bitmap *use-bitmap*))
  (let ((filtered
          (loop
            :for p :across isochrone
            :for old-pos = (when p (routepoint-position (routepoint-predecessor p)))
            :for new-pos = (when p (routepoint-position p))
            :when (and p
                       ;; (meets-all constraints new-pos old-pos)
                       ;; (check-limits old-pos new-pos limits)
                       (notany (lambda (zone) (point-in-poly-p new-pos zone)) zones)  
                       (cond (use-bitmap
                              (not (bm-is-land new-pos)))
                             (t
                              (not (intersects-land-p old-pos new-pos)))))
              :collect p)))
    (make-array (length filtered) :initial-contents filtered)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
