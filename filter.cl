;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2021-05-24 01:39:37>

(in-package :virtualhelm)

(defun-t bucket fixnum ((origin-angle double-float) (delta-angle double-float))
  (truncate (normalize-heading origin-angle) delta-angle))

(defun-t add-routepoint null ((predecessor routepoint)
                              (start-pos latlng)
                              (position latlng)
                              (delta-angle double-float)
                              (left double-float)
                              step-time
                              heading
                              speed
                              sail
                              reason
                              wind-dir
                              wind-speed)
  (declare (special next-isochrone))
  (let*
      ((maxpoints (length next-isochrone))
       (offset (bucket left delta-angle))
       (origin-distance (course-distance start-pos position))
       (origin-angle (course-angle-d start-pos position origin-distance))
       (bucket (bucket origin-angle delta-angle)))

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
      (setf (aref next-isochrone bucket)
            (create-routepoint predecessor
                               position
                               step-time
                               heading
                               nil
                               speed
                               sail
                               reason
                               wind-dir
                               wind-speed
                               origin-angle
                               origin-distance))))
  (values))


(defvar *use-bitmap* t)

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
