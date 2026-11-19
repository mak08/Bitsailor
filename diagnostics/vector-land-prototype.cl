;;; Experimental exact-vector cache for isolated benchmarks, not production startup.
;;; Load after Bitsailor. Call ROUTER::BENCHMARK-VECTOR-LAND-ACCESS on a single leg.
(in-package :cl-map)
(cffi:defcfun ("OGR_L_SetSpatialFilterRect" perf-filter-rect) :void
  (layer :pointer) (min-x :double) (min-y :double) (max-x :double) (max-y :double))
(cffi:defcfun ("OGR_L_GetFeature" perf-get-feature) :pointer (layer :pointer) (fid :int64))
(cffi:defcfun ("OGR_F_GetFID" perf-feature-id) :int64 (feature :pointer))
(cffi:defcfun ("OGR_G_Clone" perf-clone-geometry) :pointer (geometry :pointer))
(cffi:defcfun ("OGR_G_WkbSize" perf-wkb-size) :int (geometry :pointer))
(cffi:defcfun ("OGRCreatePreparedGeometry" perf-prepare) :pointer (geometry :pointer))
(cffi:defcfun ("OGRDestroyPreparedGeometry" perf-destroy-prepared) :void (geometry :pointer))
(cffi:defcfun ("OGRPreparedGeometryIntersects" perf-prepared-intersects) :int
  (prepared :pointer) (geometry :pointer))

(defstruct perf-vector-item id geometry prepared size prev next)
(defstruct perf-vector-cache map segment (cell 0.25d0) (limit (* 4 1024 1024))
  (tiles (make-hash-table :test #'equal)) (features (make-hash-table))
  (seen (make-hash-table)) head tail (bytes 0) (loads 0) (tile-loads 0) (checks 0))
(defvar *perf-vector-cache* nil)

(defun perf-vector-unlink (cache item)
  (let ((prev (perf-vector-item-prev item)) (next (perf-vector-item-next item)))
    (if prev (setf (perf-vector-item-next prev) next) (setf (perf-vector-cache-head cache) next))
    (if next (setf (perf-vector-item-prev next) prev) (setf (perf-vector-cache-tail cache) prev))))
(defun perf-vector-touch (cache item)
  (unless (eq item (perf-vector-cache-head cache))
    (when (or (perf-vector-item-prev item) (perf-vector-item-next item)
              (eq item (perf-vector-cache-tail cache)))
      (perf-vector-unlink cache item))
    (setf (perf-vector-item-prev item) nil
          (perf-vector-item-next item) (perf-vector-cache-head cache))
    (when (perf-vector-cache-head cache)
      (setf (perf-vector-item-prev (perf-vector-cache-head cache)) item))
    (unless (perf-vector-cache-tail cache) (setf (perf-vector-cache-tail cache) item))
    (setf (perf-vector-cache-head cache) item))
  item)
(defun perf-vector-free (item)
  (perf-destroy-prepared (perf-vector-item-prepared item))
  (ogr-g-destroy-geometry (perf-vector-item-geometry item)))
(defun perf-vector-close (cache)
  (maphash (lambda (id item) (declare (ignore id)) (perf-vector-free item))
           (perf-vector-cache-features cache))
  (clrhash (perf-vector-cache-features cache))
  (ogr-g-destroy-geometry (perf-vector-cache-segment cache)))
(defun perf-vector-add (cache id geometry)
  (let* ((copy (perf-clone-geometry geometry)) (prepared (perf-prepare copy)))
    (when (cffi:null-pointer-p prepared)
      (ogr-g-destroy-geometry copy) (error "Cannot prepare land geometry"))
    (let ((item (make-perf-vector-item :id id :geometry copy :prepared prepared :size (perf-wkb-size copy))))
      (incf (perf-vector-cache-loads cache))
      (incf (perf-vector-cache-bytes cache) (perf-vector-item-size item))
      (setf (gethash id (perf-vector-cache-features cache)) item)
      (perf-vector-touch cache item)
      ;; Serialized payload is a budget proxy, not a bound on native GEOS memory.
      ;; One oversize geometry is retained in this experimental implementation.
      (loop while (and (> (perf-vector-cache-bytes cache) (perf-vector-cache-limit cache))
                       (not (eq (perf-vector-cache-tail cache) item))) do
        (let ((old (perf-vector-cache-tail cache)))
          (perf-vector-unlink cache old)
          (remhash (perf-vector-item-id old) (perf-vector-cache-features cache))
          (decf (perf-vector-cache-bytes cache) (perf-vector-item-size old))
          (perf-vector-free old)))
      item)))
(defun perf-vector-get (cache id)
  (let ((item (gethash id (perf-vector-cache-features cache))))
    (if item (perf-vector-touch cache item)
        (let ((feature (perf-get-feature (mapp-polygons (perf-vector-cache-map cache)) id)))
          (when (cffi:null-pointer-p feature) (error "Missing land feature ~a" id))
          (unwind-protect (perf-vector-add cache id (ogr-f-get-geometry-ref feature))
            (ogr-f-destroy feature))))))
(defun perf-vector-tile (cache x y)
  (let ((key (cons x y)))
    (multiple-value-bind (ids found) (gethash key (perf-vector-cache-tiles cache))
      (if found ids
          (let ((layer (mapp-polygons (perf-vector-cache-map cache))) (cell (perf-vector-cache-cell cache))
                (ids nil))
            (incf (perf-vector-cache-tile-loads cache))
            (perf-filter-rect layer (* x cell) (* y cell) (* (1+ x) cell) (* (1+ y) cell))
            (ogr-l-reset-reading layer)
            (loop for feature = (ogr-l-get-next-feature layer) until (cffi:null-pointer-p feature) do
              (unwind-protect
                   (let ((id (perf-feature-id feature)))
                     (push id ids)
                     (unless (gethash id (perf-vector-cache-features cache))
                       (perf-vector-add cache id (ogr-f-get-geometry-ref feature))))
                (ogr-f-destroy feature)))
            ;; Tile IDs contain no pointers, so feature eviction cannot dangle them.
            (when (>= (hash-table-count (perf-vector-cache-tiles cache)) 4096)
              (clrhash (perf-vector-cache-tiles cache)))
            (setf (gethash key (perf-vector-cache-tiles cache)) ids))))))
(defun perf-vector-land-p (start end)
  ;; Preserve the current router's coarse endpoint-tile gate for this comparison.
  (when (tile-intersects-land-p (latlng-lat end) (latlng-lng end))
    (let ((cache *perf-vector-cache*))
      (bordeaux-threads:with-lock-held (+map-lock+)
        (let* ((segment (perf-vector-cache-segment cache)) (cell (perf-vector-cache-cell cache))
               (x0 (latlng-lng start)) (x1 (latlng-lng end))
               (y0 (latlng-lat start)) (y1 (latlng-lat end)))
          (ogr-g-set-point-2d segment 0 x0 y0)
          (ogr-g-set-point-2d segment 1 x1 y1)
          (clrhash (perf-vector-cache-seen cache))
          ;; Cover the complete query box, including segments crossing cache cells.
          (loop for y from (floor (min y0 y1) cell) to (floor (max y0 y1) cell) do
            (loop for x from (floor (min x0 x1) cell) to (floor (max x0 x1) cell) do
              (dolist (id (perf-vector-tile cache x y))
                (unless (gethash id (perf-vector-cache-seen cache))
                  (setf (gethash id (perf-vector-cache-seen cache)) t)
                  (incf (perf-vector-cache-checks cache))
                  (when (plusp (perf-prepared-intersects
                               (perf-vector-item-prepared (perf-vector-get cache id)) segment))
                    (return-from perf-vector-land-p t)))))))))))

(in-package :bitsailor)
(defun benchmark-vector-land-access (routing &key (payload-mib 4) (cell 0.25d0))
  "Compare current and cached exact land predicates in an isolated Lisp process.
Temporarily replaces FILTER-ISOCHRONE and restores it on every exit. Do not run
inside a serving application: unrelated threads would see the temporary hook."
  (let ((original (symbol-function 'filter-isochrone)) (reference nil))
    (labels ((run (label)
               (let* ((request (copy-routing routing))
                      (*routing-wave-state* (make-hash-table :test #'equal)))
                 (setf (routing-twa-angles request) (copy-seq (routing-twa-angles routing)))
                 (let* ((info (get-route-leg request)) (stats (routeinfo-stats info)))
                   (format t "~&VECTOR-PERF ~a: ~,4fs ~d candidates ~,0f p/s~%" label
                     (routestats-calctime stats) (routestats-points stats)
                     (/ (routestats-points stats) (routestats-calctime stats)))
                   (finish-output)
                   (when reference
                     (assert (= (routestats-points (routeinfo-stats reference)) (routestats-points stats)))
                     (assert (equal (with-output-to-string (s) (json s (routeinfo-best reference)))
                                    (with-output-to-string (s) (json s (routeinfo-best info)))))
                     (assert (equal (with-output-to-string (s) (json s (routeinfo-isochrones reference)))
                                    (with-output-to-string (s) (json s (routeinfo-isochrones info))))))
                   info))))
      (run "baseline-first")
      (setf reference (run "baseline-warm"))
      (let* ((cl-map::*perf-vector-cache*
               (cl-map::make-perf-vector-cache :map cl-map::*map* :segment (cl-map::ogr-g-create-geometry 2)
                                              :cell cell :limit (* payload-mib 1024 1024))))
        (unwind-protect
             (progn
               ;; Recompile only the predicate call in the existing filter. This
               ;; handles its inlined CL-MAP wrapper without changing pruning.
               (let ((*package* (find-package :bitsailor)))
                 (with-open-file (s (merge-pathnames "filter.cl" *source-root*))
                   (loop for form = (read s nil :eof) until (eq form :eof)
                         when (and (consp form) (eq (second form) 'filter-isochrone)) do
                           (eval (subst 'cl-map::perf-vector-land-p 'cl-map:intersects-land-p form))
                           (return))))
               (run "prepared-first")
               (run "prepared-warm")
               (format t "~&VECTOR-CACHE ~d tiles ~d features ~,2f MiB WKB payload, ~d feature loads, ~d predicates~%"
                 (hash-table-count (cl-map::perf-vector-cache-tiles cl-map::*perf-vector-cache*))
                 (hash-table-count (cl-map::perf-vector-cache-features cl-map::*perf-vector-cache*))
                 (/ (cl-map::perf-vector-cache-bytes cl-map::*perf-vector-cache*) 1048576d0)
                 (cl-map::perf-vector-cache-loads cl-map::*perf-vector-cache*)
                 (cl-map::perf-vector-cache-checks cl-map::*perf-vector-cache*)))
          (setf (symbol-function 'filter-isochrone) original)
          (cl-map::perf-vector-close cl-map::*perf-vector-cache*))))))

(defun capture-vector-land-checks (routing filename)
  "Capture detailed land queries from a single-leg search for the Python replay.
Use only in an isolated process. Restore the original filter on every exit."
  (let ((original (symbol-function 'filter-isochrone)) (count 0)
        (*routing-wave-state* (make-hash-table :test #'equal)))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (unwind-protect
           (progn
             (setf (symbol-function 'filter-isochrone)
                   (lambda (iso &rest args)
                     (loop for rp across iso when rp do
                       (let ((a (routepoint-position (routepoint-predecessor rp))) (b (routepoint-position rp)))
                         (when (cl-map::tile-intersects-land-p (latlng-lat b) (latlng-lng b))
                           (incf count)
                           (format s "~,12f,~,12f,~,12f,~,12f~%"
                             (latlng-lng a) (latlng-lat a) (latlng-lng b) (latlng-lat b)))))
                     (apply original iso args)))
             (values (get-route-leg (copy-routing routing)) count))
        (setf (symbol-function 'filter-isochrone) original)))))
