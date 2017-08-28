;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-08-24 20:08:18>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map file and dataset

;; Vector map describing Land areas. It must contain a layer "Land_Polygons".
;; It must have an accompanying index file land_polygons.shx."

(defvar *map-file* "/home/michael/Maps/land-polygons-split-4326/land_polygons.shp"
  "Map data filename")

(defvar *map* nil
  "The OGR Layer containing land polygons")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENSURE-MAP
;;   Initialize GDAL and load map data.
;;   Does NOT clean-up the dataset 

(defun ensure-map (&key (filename *map-file*))
  (unless *map*
    (gdal-all-register)
    (let* ((ds (gdal-open-ex filename
                             4
                             (cffi:null-pointer)
                             (cffi:null-pointer)
                             (cffi:null-pointer)))
           (land-polygons (gdal-dataset-get-layer-by-name ds "Land_Polygons")))
    (cond
      ((< 0 (ogr-l-test-capability land-polygons OLCFastSpatialFilter))
       ;; Succeed only if we have an index
       (setf *map* land-polygons))
      (t
       (error "Layer not found or no index"))))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST-POINT
;;  Return TRUE iff (lat. lon) is on land

(let ((point (ogr-g-create-geometry wkbPoint)))
  (defun test-point (lat lon)
    (ogr-g-set-point-2d point 0 lon lat)
    (ogr-l-set-spatial-filter *map* point)
    (ogr-l-reset-reading *map*)
    (loop
       :for feature = (ogr-l-get-next-feature *map*)
       :while (not (null-pointer-p feature))
       :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                  (found (ogr-g-contains polygon point)))
             (ogr-f-destroy feature)
             (when found 
               (return-from test-point t)))))
  (values nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize map on load

(ensure-map)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
