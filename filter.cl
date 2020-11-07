;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2020-11-06 23:57:40>

(declaim (optimize (speed 3) (debug 1) (space 0) (safety 0)))

(in-package :virtualhelm)


(defstruct criterion distfn compfn)

(defvar +max-origin+
  (make-criterion :distfn #'routepoint-origin-distance
                  :compfn #'>))

(defvar +min-destination+
  (make-criterion :distfn #'routepoint-destination-distance
                  :compfn #'<))

(defun filter-isochrone (isochrone
                         left
                         right
                         max-points
                         &key
                           (criterion +max-origin+)
                           (constraints nil)
                           (limits nil)
                           (fan 180d0))
  ;; (return-from filter-isochrone isochrone)
  (log2:trace-more "Filter: ~a points" (length isochrone))
  (when (= 0 (length isochrone))
    (return-from filter-isochrone nil))
  (let* ((last
          (1- (length isochrone)))
         (delta-angle
          (/ fan max-points))
         (result
          (make-array (+ max-points) :initial-element nil))) 
    (assert (plusp delta-angle))
    (flet ((bucket (routepoint)
             (let ((key-angle
                    (or (routepoint-sort-angle% routepoint)
                        (setf (routepoint-sort-angle% routepoint)
                              (let ((angle (normalize-heading
                                            (routepoint-origin-angle routepoint))))
                                angle)))))
               (min (max 0
                         (truncate (normalize-heading (- key-angle left))
                                   delta-angle))
                    (1- (length result))))))
      (loop
         :for point :across isochrone
         :do (let ((bucket (bucket point)))
               (if (>= bucket (length result))
                   ;; (log2:error "Invalid bucket ~a, angle ~a, left ~a, right ~a" bucket (routepoint-sort-angle% point) left right)
                   t
                   (when (or (null (aref result bucket))
                             (funcall (criterion-compfn criterion)
                                      (funcall (criterion-distfn criterion) point)
                                      (funcall (criterion-distfn criterion) (aref result bucket))))
                     (setf (aref result bucket) point))))))
    (let ((filtered
           (loop
              :for p :across result
              :for old-pos = (when p (routepoint-position (routepoint-predecessor p)))
              :for new-pos =  (when p (routepoint-position p))
              :when (and p
                         (meets-all constraints new-pos old-pos)
                         (check-limits old-pos new-pos limits)
                         (not (intersects-land-p old-pos new-pos)))
              :collect p)))
      (values (make-array (length filtered) :initial-contents filtered)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
