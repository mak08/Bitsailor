;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2026-04-03 22:51:52>

(in-package :bitsailor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A routing stores the start and destination of the route
;;; and other routing parameters.

(define-condition request-error (error)
  ((request :initarg :request :reader request)
   (message :initarg :message :reader message))
  (:report
   (lambda (c s)
     (format s "~a" (message c)))))


(defstruct posinfo time position speed course)

(defstruct gate
  "A gate between the endpoints A and B (latlng). NEXT is the position the route
continues to after the gate; it is used to choose between several crossings."
  a b next)

(defstruct routing
  forecast-model
  polars
  use-waves
  currents
  ;; buffer for TWA angles plus 2 best VMG angles
  twa-angles
  stepmax
  (fan *max-angle*)
  options
  starttime
  start
  ;; Intermediate targets in order before DEST: a latlng (hit exactly) or a GATE (to be crossed)
  ;; Gate (struct GATE) that the current leg has to cross instead of reaching DEST exactly
  gate
  waypoints
  dest
  box
  %dist)

;;; Compute and cache the start-to-destination great-circle distance in metres.
(defmethod routing-dist (routing)
  (or (routing-%dist routing)
      (setf  (routing-%dist routing)
             (course-distance (routing-start routing)
                              (routing-dest routing)))))

;;; Write a compact routing representation to STREAM for printing and diagnostics.
(defmethod print-object ((thing routing) stream)
  (format stream "{D=~a T=~a Fan=~a TWA=~a"
          (routing-stepmax thing)
          (routing-starttime thing)
          (routing-fan thing)
          (routing-twa-angles thing)))

(defstruct duration days hours minutes seconds)
;;; Write a compact duration representation to STREAM for printing and diagnostics.
(defmethod print-object ((thing duration) stream)
  (format stream "~dd ~2,'0dh ~2,'0dm"
          (duration-days thing)
          (duration-hours thing)
          (duration-minutes thing)))

;;; Return the matching options-list tail when the foil option is present, otherwise NIL.
(defun routing-foils (routing)
  (member "foil" (routing-options routing) :test #'string=))
;;; Return the matching options-list tail when the hull option is present, otherwise NIL.
(defun routing-hull (routing)
  (member "hull" (routing-options routing) :test #'string=))
;;; Return the matching options-list tail when the winch option is present, otherwise NIL.
(defun routing-winches (routing)
  (member "winch" (routing-options routing) :test #'string=))

(defstruct routestats start duration sails min-wind max-wind min-twa max-twa calctime steps points)

;;; Write a compact routestats representation to STREAM for printing and diagnostics.
(defmethod print-object ((thing routestats) stream)
  (format stream "D:~a T:~,2f S:~a"
          (routestats-duration thing)
          (routestats-calctime thing)
          (format-datetime nil (routestats-start thing))))

(defstruct routeinfo status best polars options stats tracks isochrones)

;;; Write a compact routeinfo representation to STREAM for printing and diagnostics.
(defmethod print-object ((thing routeinfo) stream)
  (let ((stats (or (routeinfo-stats thing)
                   (make-routestats))))
    (format stream "#<RouteInfo Start ~a Duration ~a Isochrones ~a>"
            (routestats-start stats)
            (routestats-duration stats)
            (length (routeinfo-isochrones thing)))))


(defstruct isochrone center cycle offset path stage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Isochrones are described by sets of routepoints.
;;;
;;; ### Think of a good sorting/data structure to support finding the most advanced point in a sector

(defstruct (routepoint
             (:constructor create-routepoint (predecessor position time twa heading &optional destination-distance speed sail wind-dir wind-speed curnt-dir curnt-speed (origin-angle 0) (origin-distance 0) wave-height wave-dir wave-factor)))
  predecessor
  position
  time
  twa
  heading
  destination-distance
  speed
  sail
  wind-dir
  wind-speed
  curnt-dir
  curnt-speed
  origin-angle
  origin-distance
  wave-height
  wave-dir
  wave-factor)

(defstruct trackpoint
  time position heading dtf speed sail crntdir crntspd twd tws twa wave-height wave-dir wave-factor)

;;; Convert a predecessor-chain point for output, using SUCCESSOR for the outgoing leg's heading,
;;; speed, sail and waves.
(defun create-trackpoint (routepoint successor)
  (make-trackpoint :time (routepoint-time routepoint)
                   :position (routepoint-position routepoint)
                   :heading (routepoint-heading successor)
                   :dtf (routepoint-destination-distance routepoint)
                   :speed (routepoint-speed successor)
                   :sail (routepoint-sail successor)
                   :crntdir (routepoint-curnt-dir routepoint)
                   :crntspd (routepoint-curnt-speed routepoint)
                   :twd (routepoint-wind-dir routepoint)
                   :tws (routepoint-wind-speed routepoint)
                   :twa (routepoint-twa successor)
                   ;; Waves apply to the leg from ROUTEPOINT to SUCCESSOR
                   :wave-height (routepoint-wave-height successor)
                   :wave-dir (routepoint-wave-dir successor)
                   :wave-factor (routepoint-wave-factor successor)))

;;; Write a compact routepoint representation to STREAM for printing and diagnostics.
(defmethod print-object ((thing routepoint) stream)
  (format stream "<~a@~a|~a|~a>"
          (routepoint-position thing)
          (routepoint-time thing)
          (routepoint-twa thing)
          (routepoint-curnt-dir thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Time

(defun format-timespec-datehh (stream timestamp &key (timezone +utc-zone+) (offset 0))
  "Format timestamp as YYYY-MM-DD, HH Uhr"
  (format-timestring stream
                     (adjust-timestamp timestamp (offset :hour offset))
                     :format '((:year 4) "-" (:month 2) "-" (:day 2) ", " (:hour 2) "Uhr") :timezone timezone))  

;;; Write UTC hours and minutes with a Z suffix, or return a string when STREAM is NIL.
(defun format-time (stream timestamp)
  (format stream "~2,'0d:~2,'0dZ"
          (timestamp-hour timestamp :timezone +utc-zone+)
          (timestamp-minute timestamp :timezone +utc-zone+)))

;;; Write the UTC calendar date as YYYY-MM-DD, or return a string when STREAM is NIL.
(defun format-date (stream timestamp)
  (format stream "~4,'0d-~2,'0d-~2,'0d"
          (timestamp-year timestamp :timezone +utc-zone+)
          (timestamp-month timestamp :timezone +utc-zone+)
          (timestamp-day timestamp :timezone +utc-zone+)))

;;; Format date and time using TIMEZONE (UTC by default); the output always includes a literal Z
;;; suffix.
(defun format-datetime (stream timestamp &key (timezone +utc-zone+))
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)
          (timestamp-second timestamp :timezone timezone)))

;;; Parse a date and time separated by T as UTC, allowing omitted seconds and an optional trailing
;;; Z.
(defun parse-datetime (string)
  (destructuring-bind (date time)
      (cl-utilities:split-sequence #\T string)
    (destructuring-bind (y m d)
        (cl-utilities:split-sequence #\- date)
      (destructuring-bind (utime &optional (tz ""))
          (cl-utilities:split-sequence #\Z time)
        (destructuring-bind (hr min &optional (sec "0"))
            (cl-utilities:split-sequence #\: utime)
          (assert (equal tz ""))
          (encode-timestamp 0
                            (read-from-string sec)
                            (read-from-string min)
                            (read-from-string hr)
                            (read-from-string d)
                            (read-from-string m)
                            (read-from-string y)
                            :timezone +utc-zone+))))))

;;; Write a compact timestamp representation to STREAM for printing and diagnostics.
(defmethod print-object ((thing timestamp) stream)
  (format-rfc1123-timestring stream thing))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
