;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Meteorological Data
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-08-25 00:38:39>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes
;;; * Pro:
;;;   - Allow integration of many type of forecasts
;;; * Con:
;;;   - Need to create a fresh FORECAST instance each time a forecast is requested?
;;;     (should be OK for isochrones algorithm)
;;;   - Lots of boilerplate needed for the dummy implementation?

;;; - Interface still incomplete: how to handle spatial interpolation?
;;;   - include access to i-inc/j-inc ?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ===========
;;; Datasources
;;; ===========
;;; Returns a forecast bundle from the datasource.
;;; Methods should use EQL-specializers on the forecast-bundle class name.

(defgeneric get-forecast-bundle (datasource)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; =====================
;;; Class forecast-bundle
;;; =====================
;;; A forecast-bundle provides forecast data (u+v values) over a period of time
;;; for a "rectangular" area (usually the whole world). 

(defclass forecast-bundle ()
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get the time of the first forecast in the bundle
(defgeneric fcb-time (forecast-bundle)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get the forecast for a specified time 
(defgeneric get-forecast (forecast-bundle utc-time)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ==============
;;; Class forecast
;;; ==============
;;; A forecast represents forecast data for a specific point in time.
;;; Wind speed and direction for any point in the covered area can be obtained
;;; from a forecast, but the actual values are nit computed beforehand.

(defclass forecast ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get the forecast time
(defgeneric fc-time (forecast)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Obtain wind forecast data (direction, speed)
;;;
;;; Speed is in m/s,
;;; Direction is in deg; north is 0/360; clockwise.
;;;
;;; TBD: Should this method allow only latlng on grid points or interpolate?
(defgeneric get-wind-forecast (forecast latlng))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ====================
;;; Dummy implementation
;;; ====================

(defclass constant-wind-bundle (forecast-bundle)
  ((fcb-time :reader fcb-time :initarg :time)))

(defmethod get-forecast-bundle ((datasource (eql 'constant-wind-bundle)))
  (make-instance 'constant-wind-bundle :time (now)))

(defclass constant-wind-forecast (forecast)
  ())

(defmethod get-forecast ((bundle constant-wind-bundle) (utc-time local-time:timestamp))
  (make-instance 'constant-wind-forecast))

(defmethod get-wind-forecast ((forecast constant-wind-forecast) latlng)
  (values (latlng-lng latlng)
          (/ (+ (latlng-lat latlng) 90) 10)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
