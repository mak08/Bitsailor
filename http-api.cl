;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-09-20 00:15:05>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP API

(defparameter *session-ht* (make-hash-table :test #'equalp))

(defstruct session
  (session-id (make-session-id))
  (routing (make-routing)))

(defun find-or-create-session (request response)
  (let* ((session-cookie
          (get-cookie request "SessionID"))
         (session-id)
         (session))
    (cond (session-cookie
           (setf session-id (cookie-value session-cookie))
           (let ((stored-session (gethash session-id *session-ht*)))
             (cond
               ((null stored-session)
                (setf session (setf (gethash session-id *session-ht*)
                                    (make-session :session-id session-id)))
                (log2:info "Session was lost for SessionID ~a." session-id))
               (t
                (setf session stored-session)
                (log2:info "Session retrieved for SessionID ~a." session-id)))))
          (t
           (setf session-id (make-session-id))
           (set-cookie response "SessionID" session-id)
           (setf session (setf (gethash session-id *session-ht*)
                               (make-session :session-id session-id)))
           (log2:info "Session created for new SessionID ~a." session-id)))
    session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setRoute

(defun |setRoute| (location request response &key |pointType| |lat| |lng|)
  (declare (ignore location))
  (handler-case
      (let* ((session (find-or-create-session request response))
             (lat (coerce (read-from-string |lat|) 'double-float))
             (lng (coerce (read-from-string |lng|) 'double-float))
             (position (make-latlng :lat lat :lng lng)))
        (log2:info "~a: lat ~a, lng ~a." |pointType| |lat| |lng|)
        (log2:trace "Session: ~a, Request: ~a" session request)
        (cond ((is-land lat lng)
               (error "Can't sail ~:[to~;from~] interior point ~a" (string= |pointType| "start") position ))
              (t
               (let ((routing (session-routing session)))
                 (cond
                   ((string= |pointType| "start")
                    (setf (routing-start routing) position))
                   ((string= |pointType| "dest")
                    (setf (routing-dest routing) position)))
                 (setf (http-body response)
                       (with-output-to-string (s)
                         (json s session)))))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setParameter
(defun |setParameter| (location request response &key |name| |value|)
  (declare (ignore location))
  (let* ((session (find-or-create-session request response)))
    (let ((routing (session-routing session)))
      (cond
        ((string= |name| "forecastbundle")
         (cond
           ((string= |value| "dwd025")
            (setf (routing-forecast-bundle routing) 'dwd-icon-bundle))
           ((string= |value| "noaa")
            (setf (routing-forecast-bundle routing) 'noaa-bundle))
           (t
            (error "Invalid forecast designator ~a" |value|))))
        ((string= |name| "polars")
         (setf (routing-polars routing) |value|))
        ((string= |name| "duration")
         (let ((duration-hrs
                (read-from-string |value|)))
           (setf (routing-stepmax routing)
                 (* duration-hrs 3600))))
        ((string= |name| "searchangle")
         (let ((fan (read-from-string |value|)))
           (setf (routing-fan routing) fan)))
        ((string= |name| "pointsperisochrone")
         (let ((points-per-isochrone
                (read-from-string |value|)))
           (setf (routing-max-points-per-isochrone routing)
                 points-per-isochrone)))
        
        (t
         (log2:error "Unsupported parameter ~a ~a" |name| |value|)
         (error "unsupported")))
      (setf (http-body response)
            (with-output-to-string (s)
              (json s session))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getRoute

(defun |getRoute| (location request response)
  (declare (ignore location))
  (handler-case
      (let* ((session
              (find-or-create-session request response))
             (routing
              (session-routing session))
             (routeinfo
              (get-route routing)))
        (setf (http-body response)
              (with-output-to-string (s)
                (json s routeinfo))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getSession

(defun |getSession| (location request response)
  (declare (ignore location))
  (let* ((session
          (find-or-create-session request response)))
      (setf (http-body response)
            (with-output-to-string (s)
              (json s session)))))
         
(defun make-session-id ()
  (create-uuid))

(defun create-uuid ()
  (with-open-file (f "/proc/sys/kernel/random/uuid")
    (read-line f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getWind

;; Get wind data in indicated (Google Maps) coordinates : [-90, 90], (-180,180].
;; Returns (0d0, -1d0) for unavailable values.
;; Does not work if date line is in longitude range.
(defun |getWind| (location request response &key  (|time| nil) (|offset| "0") |north| |east| |west| |south| (|ddx| "0.5") (|ddy| "0.5"))
  (declare (ignore location request))
  (handler-case
      (let ((*read-default-float-format* 'double-float))
        (let* ((session (find-or-create-session request response))
               (routing (session-routing session))
               (forecast-bundle (or (get-forecast-bundle (routing-forecast-bundle routing))
                                    (get-forecast-bundle 'constant-wind-bundle)))
               (forecast-time
                (if |time|
                    (parse-rfc3339-timestring |time|)
                    (adjust-timestamp
                        (fcb-time forecast-bundle)
                      (:offset :hour (read-from-string |offset|)))))
               (forecast (get-forecast forecast-bundle forecast-time))
               (ddx (read-from-string |ddx|))
               (ddy (read-from-string |ddy|))
               (north (read-from-string |north|))
               (south (read-from-string |south|))
               (east (read-from-string |east|))
               (west (read-from-string |west|)))
          (assert (and (plusp ddx)
                       (plusp ddy)
                       (< south north)))
          (setf (http-body response)
                (with-output-to-string (s)
                  (json s
                        (list
                         (format-timespec-datehh nil (fcb-time forecast-bundle) :timezone *default-timezone*)
                         (format-timespec-datehh nil forecast-time :timezone *default-timezone*)
                         (loop
                            :for lat :from north :downto south :by ddy
                            :collect (loop
                                        :for lon :from west :to east :by ddx
                                        :collect (multiple-value-bind (dir speed)
                                                     (let ((lon-icon
                                                            (if (< lon 0) (+ lon 360) lon)))
                                                       (get-wind-forecast forecast (make-latlng :lat lat :lng lon-icon)))
                                                   (list (round-to-digits dir 2)
                                                         (round-to-digits speed 2)))))))))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))
    #+ccl(ccl::invalid-memory-access (e)
           (log2:error "(|getWind| :north ~a :east ~a :west ~a :south ~a): ~a"  |north| |east| |west| |south| e)
           (setf (status-code response) 500)
           (setf (status-text response) (format nil "~a" e)))))

#|
(defun |getTWAPath| (location request response &key |offset| |lat| |lon| |heading| |time|)
  (setf (http-body response)
        (with-output-to-string (s)
          (let ((*read-default-float-format* 'double-float))
            (let ((offset (read-from-string |offset|)))
              (json s (twa (now)
                           offset
                           (make-latlng :lat (coerce (read-from-string |lat|) 'double-float)
                                        :lng (coerce (read-from-string |lon|) 'double-float)) 
                           (read-from-string |time|)
                           +10min+)))))))
|#

;; This must be done in the config file.
;; polarcl:reset deletes registered function,
;; (register-function 'dwd:|getWind|)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
