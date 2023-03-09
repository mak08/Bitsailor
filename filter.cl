;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2023-03-06 22:24:37>

(in-package :bitsailor)

(defun-t bucket fixnum ((origin-angle double-float) (delta-angle double-float))
  (truncate origin-angle delta-angle))
  
(declaim (inline add-routepoint))
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
                              reason
                              penalty-time
                              wind-dir
                              wind-speed)
  (declare (special next-isochrone max-dist min-angle))
  (let*
      ((maxpoints (length next-isochrone))
       (offset (bucket left delta-angle))
       (bucket (bucket origin-angle delta-angle))
       (max-bucket (bucket origin-angle min-angle))
       (energy (energy predecessor reason wind-speed step-size)))
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
                                 energy
                                 reason
                                 penalty-time
                                 wind-dir
                                 wind-speed
                                 origin-angle
                                 origin-distance)))))
  (values))

(defun-t filter-isochrone vector ((isochrone vector) &key (limits nil) (use-bitmap *use-bitmap*))
  (let ((filtered
          (loop
            :for p :across isochrone
            :for old-pos = (when p (routepoint-position (routepoint-predecessor p)))
            :for new-pos = (when p (routepoint-position p))
            :when (and p
                       ;; (meets-all constraints new-pos old-pos)
                       (check-limits old-pos new-pos limits)
                       (cond (use-bitmap
                              (not (bm-is-land new-pos)))
                             (t
                              (not (intersects-land-p old-pos new-pos)))))
              :collect p)))
    (make-array (length filtered) :initial-contents filtered)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
