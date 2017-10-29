;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-10-30 00:39:17>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map file and dataset

;; Vector map describing Land areas. It must contain a layer "Land_Polygons".
;; It must have an accompanying index file land_polygons.shx."
;;
;;   There is a smaller file, simplified-land-polygons-complete-3857,
;;   but it is NOT faster, most likely because it does not have split polygons
;;   (and therefore, indexing does not help as much).
;;   Moreover, land detection is too coarse with the simplified polygons.
(defvar *map-file* "/home/michael/Maps/land-polygons-split-4326/land_polygons.shp"
  "Map data filename")

(defvar *map* nil
  "The OGR Layer containing land polygons")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENSURE-MAP
;;   Initialize GDAL and load map data.
;;   Does NOT clean-up the dataset 

(defvar +map-lock+
  (bordeaux-threads:make-lock "MAP-LOCK"))

(defun ensure-map (&key (filename *map-file*))
  (unless *map*
    (bordeaux-threads:with-lock-held (+map-lock+)
      (log2:info "Loading shapefile ~a" filename)
      (gdal-all-register)
      (let* ((ds (gdal-open-ex filename
                               4
                               (cffi:null-pointer)
                               (cffi:null-pointer)
                               (cffi:null-pointer)))
             (layer-count (gdal-dataset-get-layer-count ds))
             (land-polygons (gdal-dataset-get-layer ds 0)))
        (unless (eql layer-count 1)
          (error "Unexpected layer count ~a" layer-count))
        (cond
          ((< 0 (ogr-l-test-capability land-polygons OLCFastSpatialFilter))
           ;; Succeed only if we have an index
           (setf *map* land-polygons))
          (t
           (error "Layer not found or no index")))))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IS-LAND
;;  Return TRUE iff (lat. lon) is on land

(let ((point (ogr-g-create-geometry wkbPoint)))
  (defun is-land (lat lon)
    (bordeaux-threads:with-lock-held (+map-lock+)
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
                 (return-from is-land t)))))))


(let ((segment (ogr-g-create-geometry wkbLineString)))
  (ogr-g-add-point-2d segment 0d0 0d0)
  (ogr-g-add-point-2d segment 0d0 0d0)
  (defun intersects-land-p (start end)
    (bordeaux-threads:with-lock-held (+map-lock+)
      (ogr-g-set-point-2d segment 0 (latlng-lng start) (latlng-lat start))
      (ogr-g-set-point-2d segment 1 (latlng-lng end) (latlng-lat end))
      (ogr-l-set-spatial-filter *map* segment)
      (ogr-l-reset-reading *map*)
      ;; setSpatialFilter may return too many features. Scanning the result may be necessary.
      ;; See http://www.gdal.org/ogr__api_8h.html#a678d1735bc82533614ac005691d1138c
      (loop
         :for feature = (ogr-l-get-next-feature *map*)
         :while (not (null-pointer-p feature))
         :do (let* ((polygon (ogr-f-get-geometry-ref feature))
                    (found (ogr-g-intersects polygon segment)))
               (ogr-f-destroy feature)
               (when found
                 (return-from intersects-land-p t)))))))

;;; Same as above, but without scanning the result.
;;; In practive this looked correct but is only slightly faster.
(let ((segment (ogr-g-create-geometry wkbLineString)))
  (ogr-g-add-point-2d segment 0d0 0d0)
  (ogr-g-add-point-2d segment 0d0 0d0)
  (defun intersects-land-noscan-p (start end)
    (bordeaux-threads:with-lock-held (+map-lock+)
      (ogr-g-set-point-2d segment 0 (latlng-lng start) (latlng-lat start))
      (ogr-g-set-point-2d segment 1 (latlng-lng end) (latlng-lat end))
      (ogr-l-set-spatial-filter *map* segment)
      (ogr-l-reset-reading *map*)
      (let ((feature (ogr-l-get-next-feature *map*)))
        (not (null-pointer-p feature))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize map on load -
;;; DON'T - *map-file* needs to be customized first!

;;; (ensure-map)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
