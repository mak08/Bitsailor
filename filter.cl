;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-12-10 10:26:56>

(in-package :virtualhelm)

(defun filter-isochrone-nosort (isochrone max-points)
  (let*
      ((min-angle
        (loop :for p :across isochrone :minimize (routepoint-origin-angle p)))
       (max-angle
        (loop :for p :across isochrone :maximize (routepoint-origin-angle p)))
       (size
        (length isochrone))
       (result
        (make-array (* (1+ max-points) 2) :initial-element nil)))
    (loop :for p :across isochrone :do (decf (routepoint-origin-angle p) min-angle))
    (loop
       :with delta-angle = (/ (- max-angle min-angle) max-points 2)
       :for point :across isochrone
       :for a = (routepoint-origin-angle point)
       :for d = (routepoint-origin-distance point)
       :for k = (truncate (abs a) delta-angle)
       :for best = (aref result k)
       :do (when (or (null best)
                     (> d (routepoint-origin-distance best)))
             (setf (aref result k) point))
       :finally (return result))))

(defun filter-isochrone (isochrone max-points
                         &key (goalfn (lambda (p q)
                                        ())))
  (let* ((last
          (1- (length isochrone)))
         (a-start
          (routepoint-origin-angle (aref isochrone 0)))
         (a-end
          (routepoint-origin-angle (aref isochrone last)))
         (southbound
          (< a-end a-start))
         (h-end
          (if southbound  (+ a-end 360) a-end))
         (delta-angle
          (/ (- h-end a-start) max-points))
         (a0
          a-start)
         (dmin 
          (routepoint-destination-distance (aref isochrone 0)))
         (kmin
          0)
         (result
          (make-array 0 :fill-pointer 0 :adjustable t)))
    (assert (plusp delta-angle))
    (flet ((routepoint-sort-key (routepoint)
             (or (routepoint-sort-angle% routepoint)
                 (setf (routepoint-sort-angle% routepoint)
                       (let ((angle (routepoint-origin-angle routepoint)))
                         (check-type angle angle)
                         (if (and southbound (< angle 0)) (+ angle 360) angle))))))
      (setf isochrone (sort isochrone #'< :key #'routepoint-sort-key))
      (loop
         :for point :across isochrone
         :for k :from 0
         :do (cond
               ((>= (abs (- (routepoint-origin-angle point) a0)) delta-angle)
                ;; Sector scanned, record best distance...
                (let* ((pmin (aref isochrone kmin))
                       (pmin-pred (routepoint-predecessor pmin)))
                  (unless (intersects-land-p (routepoint-position pmin)
                                             (routepoint-position pmin-pred))
                    (vector-push-extend (aref isochrone kmin) result)))
                ;; ... and reset.
                (when (< k last)
                  (setf kmin k)
                  (let ((next-point (aref isochrone kmin)))
                    (setf a0 (routepoint-origin-angle next-point))
                    (setf dmin (routepoint-destination-distance next-point)))))
               ((< (routepoint-destination-distance point) dmin)
                (setf kmin k)
                (setf dmin (routepoint-destination-distance point))))
         :finally (return result)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
