;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-09-14 00:49:03>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP API

(defparameter *session-ht* (make-hash-table :test #'equalp))

(defstruct session
  (session-id (make-session-id))
  (routing (make-routing))
  (direction ""))



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
  (log2:info "~a: lat ~a, lng ~a." |pointType| |lat| |lng|)
  (let* ((session (find-or-create-session request response))
         (lat (coerce (read-from-string |lat|) 'double-float))
         (lng (coerce (read-from-string |lng|) 'double-float)))
    (log2:trace "Session: ~a, Request: ~a" session request)
    (let ((routing (session-routing session)))
      (cond
        ((string= |pointType| "start")
         (setf (routing-start routing) (make-latlng :lat lat :lng lng)))
        ((string= |pointType| "dest")
         (setf (routing-dest routing) (make-latlng :lat lat :lng lng))))
      (let ((path (get-twa-path (session-routing session))))
        (setf (routing-twapath routing) path))
      (setf (session-direction session)
            (round (course-angle (routing-start routing)
                                 (routing-dest routing))))
      (setf (http-body response)
            (with-output-to-string (s)
              (json s session))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setParameter
(defun |setParameter| (location request response &key |name| |value|)
  (declare (ignore location))
  (let* ((session (find-or-create-session request response)))
    (let ((routing (session-routing session)))
      (cond
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
         (error "unsupported")))
      (setf (http-body response)
            (with-output-to-string (s)
              (json s session))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getRoute

(defun |getRoute| (location request response)
  (declare (ignore location))
  (let* ((session
          (find-or-create-session request response))
         (routing
          (session-routing session))
         (routeinfo
          (get-route routing)))
      (setf (http-body response)
            (with-output-to-string (s)
              (json s routeinfo)))))
         
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
(defun |getWind| (location request response &key (|offset| "0") |north| |east| |west| |south| (|ddx| "0.5") (|ddy| "0.5"))
  (declare (ignore location request))
  (handler-case
      (let ((*read-default-float-format* 'double-float))
        (let* (;; Recalculate offset based on forecast file age
               (offset (read-from-string |offset|))
               (forecast-bundle (or (get-forecast-bundle 'dwd-icon-bundle)
                                    (get-forecast-bundle 'constant-wind-bundle)))
               (forecast (get-forecast forecast-bundle (adjust-timestamp (now) (:offset :hour offset))))
               (ddx (read-from-string |ddx|))
               (ddy (read-from-string |ddy|))
               (north (coerce (read-from-string |north|) 'double-float))
               (south (coerce (read-from-string |south|) 'double-float))
               (east (coerce (read-from-string |east|) 'double-float))
               (west (coerce (read-from-string |west|) 'double-float)))
          (assert (and (plusp ddx)
                         (plusp ddy)
                         (< south north)))
            (setf (http-body response)
                  (with-output-to-string (s)
                    (json s
                          (list
                           (format-timespec-datehh nil (fcb-time forecast-bundle) :timezone *default-timezone*)
                           (format-timespec-datehh nil (fcb-time forecast-bundle) :offset offset :timezone *default-timezone*)
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
      (log2:error "|getWind|: ~a" e)
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
