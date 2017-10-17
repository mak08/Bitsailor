;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-10-17 21:33:07>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP API

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-page
;;;
;;; Return the main page (this is currently the only HTML page)
;;; This function is called as a dynamic handler. It is NOT registered.
(defun get-page (server handler request response)
  (handler-case 
      (let ((session (find-or-create-session request response))
            (path (merge-pathnames (make-pathname :name "index" :type "html")
                                   (make-pathname :directory (append (pathname-directory #.*compile-file-truename*)
                                                                     '("web"))))))
        (load-file path response)
        (log2:info "Query: ~a " (parameters request))
        (dolist (pair (parameters request))
          ;; Currently the only paramters supported here are 'start' and 'dest', designated by a place name.
          ;; See places.cl.
          (destructuring-bind (name value)
              pair
            (set-routing-parameter session name (find-place value)))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))
 
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
               (set-routing-parameter session |pointType| position)
               (setf (http-body response)
                     (with-output-to-string (s)
                       (json s session))))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setParameter
(defun |setParameter| (location request response &key |name| (|value| nil))
  (declare (ignore location))
  (let* ((session (find-or-create-session request response)))
    (set-routing-parameter session |name| |value|) 
    (setf (http-body response)
          (with-output-to-string (s)
            (json s session)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getWind
;;; Get wind data in indicated (Google Maps) coordinates : [-90, 90], (-180,180].
;;; Returns (0d0, -1d0) for unavailable values.
;;; Does not work if date line is in longitude range.
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
                         (format-rfc3339-timestring nil (fcb-time forecast-bundle) :timezone *default-timezone*)
                         (format-rfc3339-timestring nil forecast-time :timezone *default-timezone*)
                         (fcb-max-offset forecast-bundle)
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

(defun |getTWAPath| (location request response &key |time| |latA| |lngA| |lat| |lng|)
  (handler-case
      (let* ((*read-default-float-format* 'double-float)
             (session (find-or-create-session request response))
             (routing (session-routing session))
             (time (parse-rfc3339-timestring |time|))
             (lat-a (read-from-string |latA|))
             (lng-a (read-from-string |lngA|))
             (lat (read-from-string |lat|))
             (lng (read-from-string |lng|)))
        (setf (http-body response)
              (with-output-to-string (s)
                (json s (get-twa-path routing :time time :lat-a lat-a :lng-a lng-a :lat lat :lng lng)))))
    (error (e)
      (log2:error "~a" e)
      (setf (status-code response) 500)
      (setf (status-text response) (format nil "~a" e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

;;; Keep session table when reloading system!
(defvar *session-ht* (make-hash-table :test #'equalp))

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


(defun load-file (path response)
  (with-open-file (f path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence buffer f)
      (setf (http-body response) buffer)))
  (setf (http-header response :|Content-Type|) "text/html"))

(defun set-routing-parameter (session name value)
  (log2:info "Session ~a: ~a=~a" (session-session-id session) name value)
  (let ((routing (session-routing session)))
    (cond
      ((string= name "forecastbundle")
       (cond
         ((string= value "DWD-ICON-BUNDLE")
          (setf (routing-forecast-bundle routing) 'dwd-icon-bundle))
         ((string= value "NOAA-BUNDLE")
          (setf (routing-forecast-bundle routing) 'noaa-bundle))
         (t
          (error "Invalid forecast designator ~a" value))))
      ((string= name "starttime")
       (setf (routing-starttime routing) value))
      ((string= name "polars")
       (setf (routing-polars routing) value))
      ((string= name "foils")
       (setf (routing-foils routing) (string= value "true")))
      ((string= name "polish")
       (setf (routing-polish routing) (string= value "true")))
      ((string= name "fastmanoeuvres")
       (setf (routing-fastmanoeuvres routing) (string= value "true")))
      ((string= name "minwind")
       (setf (routing-minwind routing) (string= value "true")))
      ((string= name "duration")
       (let ((duration-hrs
              (read-from-string value)))
         (setf (routing-stepmax routing)
               (* duration-hrs 3600))))
      ((string= name "searchangle")
       (let ((fan (read-from-string value)))
         (setf (routing-fan routing) fan)))
      ((string= name "angleincrement")
       (let ((increment (read-from-string value)))
         (setf (routing-angle-increment routing) increment)))
      ((string= name "pointsperisochrone")
       (let ((points-per-isochrone
              (read-from-string value)))
         (setf (routing-max-points-per-isochrone routing)
               points-per-isochrone)))
      ((string= name "start")
       (setf (routing-start routing) value))
      ((string= name "dest")
       (setf (routing-dest routing) value))
      (t
       (log2:error "Unhandled parameter ~a ~a" name value)
       (error "Unhandled parameter ~a ~a" name value)))))
         
(defun make-session-id ()
  (create-uuid))

(defun create-uuid ()
  (with-open-file (f "/proc/sys/kernel/random/uuid")
    (read-line f)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
