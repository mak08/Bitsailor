;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-20 21:00:48>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessing GRIB wind data


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpolation
;;; - Speed
;;;   Compute speed at GRIB coordinates, then interpolate at intermediate coordinates
;;; - Direction
;;;   Interpolate U and V componentes at intermediate coordinates, then calculate direction (U/V interpolation)

(defun get-interpolated-wind (grib offset lat lon)
  "Get interpolated direction(deg) & speed(m/s) at lat, lon.
Data for offset (hour) is used and no time interpolation is done."
  (declare (double-float lat lon))

  (when (< lon 0d0)
    (incf lon 360d0))
  
  (handler-case
      (let ((i-inc (gribfile-i-inc grib))
            (j-inc (gribfile-j-inc grib)))
        (declare (double-float  i-inc j-inc))
        (with-bindings
            (((flat rlat) (ffloor lat j-inc))
             ((flon rlon) (ffloor lon i-inc)))
          (cond
            ((and (eql rlat 0d0)
                  (eql rlon 0d0))
             ;; Don't interpolate if requested coordinates fall on a grid node
             (with-bindings (((u v) (get-wind grib offset lat lon)))
               (values (angle u v)
                       (enorm u v))))
            (t
             (let*
                 ((x0 (* (ffloor lon i-inc) i-inc))
                  (x1 (+ x0 i-inc))
                  (y0 (* (ffloor lat j-inc) j-inc))
                  (y1 (+ y0 j-inc)))
               (declare (double-float x0 x1 y0 y1))
               (with-bindings (((u00 v00) (get-wind grib offset y0 x0))
                               ((u01 v01) (get-wind grib offset y0 x1))
                               ((u10 v10) (get-wind grib offset y1 x0))
                               ((u11 v11) (get-wind grib offset y1 x1)))
                 (declare (double-float x0 x1 y0 y1 u00 u01 u10 u11 v00 v10 v01 v11))
                 (let* ((u (bilinear lon lat x0 x1 y0 y1 u00 u01 u10 u11))
                        (v (bilinear lon lat x0 x1 y0 y1 v00 v01 v10 v11))
                        (s (bilinear lon lat x0 x1 y0 y1
                                     (enorm u00 v00)
                                     (enorm u01 v01)
                                     (enorm u10 v10)
                                     (enorm u11 v11))))
                   (values (angle u v)
                           s))))))))
    (error (e)
      (log2:error "Could not retrieve interpolated wind: ~a~%" e)
      (values 0d0 -1d0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grib values at grib coordinates

(declaim (ftype (function (t integer double-float double-float) (values double-float double-float)) get-wind))

(defun get-wind (grib offset lat lon)
  "Get u/v values at lat,lon (in GRIB coordinates)"
  (declare (double-float lat lon))
  (declare (ftype (function (array) (simple-array double-float)) grib-values-u-array grib-values-v-array))
  (let*
      ;; Get grib parameters to check if latlon is within grib range.
      ;; Also used for computing index into data array
      ((j-scan-pos-p (eql (gribfile-j-scan-pos grib) 1))
       (lat0 (gribfile-lat-start grib))
       (lon0 (gribfile-lon-start grib))
       (lat1 (gribfile-lat-end grib))
       (lon1 (gribfile-lon-end grib))
       (latpoints (gribfile-lat-points grib))
       (lonpoints (gribfile-lon-points grib))
       ;; Use forecast data at offset (hour). No time interpolation.
       (grib-values
        (aref (gribfile-data grib) offset)))
    (declare (double-float lat0 lat1 lon0 lon1))
    (declare ((unsigned-byte 16) lonpoints latpoints))
    ;; 0 - Check if latlon is within grib coordinates
    (unless (and (or (<= lat0 lat lat1)
                     (<= lat1 lat lat0))
                 (or (and (< lon0 lon1)
                          (<= lon0 lon lon1))
                     (or (<= lon0 lon 360)
                         (<= 0 lon lon1))))
      (error "Invalid coordinates [~a, ~a]" lat lon))
    (let
        ;; Compute values array index
        ;; 1 - offset by grib boundaries
        ((olat (if j-scan-pos-p (- lat lat0) (- lat0 lat)))
         (olon (- lon lon0)))
      ;; 2 - Normalize to Median/Parallel ranges (0-180, 0-360)
      (when (< olat 0) (incf olat 180))
      (when (< olon 0) (incf olon 360))
      ;; 3 - Divide by grid step width to obtain array index
      (let* ((i-inc (gribfile-i-inc grib))
             (j-inc (gribfile-j-inc grib))
             (lat-index (floor olat j-inc))
             (lon-index (floor olon i-inc)))
        (declare (double-float i-inc j-inc))
        (declare ((unsigned-byte 16) lat-index lon-index))
        ;; Paranoia
        (unless (<= 0 lon-index lonpoints)
          (error "Invalid data index ~a for lon ~a in ~a" lon-index lon grib-values))
        (unless (<= 0 lat-index latpoints)
          (error "Invalid data index ~a for lat ~a in ~a" lon-index lon grib-values))
        ;; 4 - Return grib data
        ;; (log2:trace "Getting latlon[~a,~a] at index[~a,~a]~%" lat lon lat-index lon-index)
        (let* ((lat-offset (* lat-index lonpoints))
               (offset (+ lat-offset lon-index)))
          (values (aref (the (simple-array double-float) (grib-values-u-array grib-values))
                        offset)
                  (aref (the (simple-array double-float) (grib-values-v-array grib-values))
                        offset)))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
