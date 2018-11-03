;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-10-28 15:01:53>

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
                         &key (criterion +max-origin+))
  ;; (return-from filter-isochrone isochrone)
  (log2:debug "Filter: ~a points" (length isochrone))
  (when (= 0 (length isochrone))
    (return-from filter-isochrone nil))
  (let* ((fan 180.0)
         (last
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
               (max 0
                    (truncate (normalize-heading (- key-angle left))
                              delta-angle)))))
      (loop
         :for point :across isochrone
         :do (let ((bucket (bucket point)))
               (if (>= bucket (length result))
                   (log2:error "Invalid bucket ~a, angle ~a, left ~a, right ~a"
                               bucket
                               (routepoint-sort-angle% point)
                               left
                               right)
                   (when (or (null (aref result bucket))
                             (funcall (criterion-compfn criterion)
                                      (funcall (criterion-distfn criterion) point)
                                      (funcall (criterion-distfn criterion) (aref result bucket))))
                     (setf (aref result bucket) point))))))
    (let ((filtered
           (loop
              :for p :across result
              :unless (and  p
                            (intersects-land-p (routepoint-position (routepoint-predecessor p))
                                               (routepoint-position p)))
              :collect p)))
      (values (make-array (length filtered) :initial-contents filtered)))))

#+()(defun filter-isochrone (isochrone
                         max-points
                         &key (criterion +max-origin+))
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
          (funcall (criterion-distfn criterion) (aref isochrone 0)))
         (kmin
          0)
         (result
          (make-array 0 :fill-pointer 0 :adjustable t)))
    (assert (plusp delta-angle))
    (flet ((routepoint-sort-key (routepoint)
             (or (routepoint-sort-angle% routepoint)
                 (setf (routepoint-sort-angle% routepoint)
                       (let ((angle (routepoint-origin-angle routepoint)))
                         ;; (check-type angle angle)
                         (if (and southbound (< angle 0)) (+ angle 360) angle))))))
      (setf isochrone (sort isochrone #'< :key #'routepoint-sort-key))
      (let ((iso
             (loop
                :for point :across isochrone
                :for k :from 0
                :do (cond
                      ((>= (abs (- (routepoint-origin-angle point) a0)) delta-angle)
                       ;; Sector scanned, record best distance...
                       (let* ((pmin (aref isochrone kmin))
                              (pmin-pred (routepoint-predecessor pmin)))
                         (unless
                             #+nil(intersects-land-p (routepoint-position pmin) (routepoint-position pmin-pred))
                             (is-land (latlng-lat (routepoint-position pmin))
                                      (latlng-lng (routepoint-position pmin)))
                           (vector-push-extend (aref isochrone kmin) result)))
                       ;; ... and reset.
                       (when (< k last)
                         (setf kmin k)
                         (let ((next-point (aref isochrone kmin)))
                           (setf a0 (routepoint-origin-angle next-point))
                           (setf dmin (funcall (criterion-distfn criterion) next-point)))))
                      ((funcall (criterion-compfn criterion)
                                (funcall (criterion-distfn criterion) point)
                         dmin)
                       (setf kmin k)
                       (setf dmin (funcall (criterion-distfn criterion) point))))
                :finally (return result))))
        iso))))


(defun southbound-p (min-heading max-heading)
  (< min-heading 180 max-heading))

(defun clip-isochrone (isochrone)
  (let ((length (length isochrone)))
    (case length
      (0
       ;; (error "Out of valid boat positions")
       nil)
      (1
       isochrone)
      (otherwise
       (loop
          :with left = 0
          :with right = length
          :for first :from 0 :to (- length 2)
          :for second = (1+ first)
          :for delta = (- (routepoint-destination-distance (aref isochrone second))
                          (routepoint-destination-distance (aref isochrone first)))
          :do (progn
                (when (> delta 20000)
                  ;; Big distance increase - clip forward
                  (setf right second)
                  (return (subseq isochrone left right)))
                (when (< delta -20000)
                  ;; Big distance decrease - clip backward
                  (setf left second)))
          :finally (return (subseq isochrone left right)))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
