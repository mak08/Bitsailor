;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-08-24 20:07:23>

(in-package :virtualhelm)

(defvar wkbPoint 1)
(defvar GDAL_OF_VECTOR 4)
(defvar OLCFastSpatialFilter "FastSpatialFilter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization & Files

(defcfun ("GDALAllRegister" gdal-all-register) :void)

(defcfun ("GDALOpenEx" gdal-open-ex) :pointer
  (filename :string)
  (open-flags :uint)
  (allowed-drivers :string)
  (open-options :string)
  (sibling-files :string))

(defcfun ("GDALClose" gdal-close) :void
  (datasource :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dataset/Datasource

(defcfun ("GDALDatasetGetLayerCount" gdal-dataset-get-layer-count) :int
  (datasource :pointer))

(defcfun ("GDALDatasetGetLayerByName"  gdal-dataset-get-layer-by-name) :pointer
  (dataset :pointer)
  (layer-name :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layer

(defcfun ("OGR_L_SetSpatialFilter" ogr-l-set-spatial-filter) :void
  (layer :pointer)
  (geom :pointer))

(defcfun ("OGR_L_ResetReading" ogr-l-reset-reading) :void
  (layer :pointer))

(defcfun ("OGR_L_TestCapability" ogr-l-test-capability) :int
  (layer :pointer)
  (capability :string))

(defcfun ("OGR_L_GetNextFeature" ogr-l-get-next-feature) :pointer
  (layer :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Feature

(defcfun ("OGR_F_GetGeometryRef" ogr-f-get-geometry-ref) :pointer
  (feature :pointer))

(defcfun ("OGR_F_Destroy" ogr-f-destroy) :void
  (feature :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometry

(defcfun ("OGR_G_CreateGeometry" ogr-g-create-geometry) :pointer
  (type :uint))

(defcfun ("OGR_G_GetGeometryType" ogr-g-get-geometry-type) :pointer
  (geom :pointer))

(defcfun ("OGR_G_Contains"  OGR-G-Contains) :int
  (container :pointer)
  (geom :pointer))

(defcfun ("OGR_G_SetPoint_2D" ogr-g-set-point-2d) :void
  (geom :pointer)
  (index :int)
  (lon :double)
  (lat :double))

;;; Geometry type

(defcfun ("OGR_GT_Flatten" ogr-gt-flatten) :pointer
  (type :uint))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
