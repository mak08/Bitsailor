;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-04-15 20:29:22>

(in-package :virtualhelm)
 
(defun parse-gprmc (m)
  ;; $GPRMC,185951.00,A,4103.4804,N,02426.2568,W,10.795927221436223,256,120421,,,A*66
  (destructuring-bind (key time a lat lat-sgn lng lng-sgn speed course date vari vari-sign &rest flags)
      (cl-utilities:split-sequence #\, m)
    (declare (ignore key a vari vari-sign flags))
    (let ((position
            (make-latlng :lat (decode-latlng lat lat-sgn)
                         :lng (decode-latlng lng lng-sgn)))
          (time (format nil "20~a-~a-~aT~a:~a:~aZ"
                        (subseq date 4 6)
                        (subseq date 2 4)
                        (subseq date 0 2)
                        (subseq time 0 2)
                        (subseq time 2 4)
                        (subseq time 4 6))))
      (make-posinfo :time time
                    :position position
                    :speed speed
                    :course course))))
  
(defun decode-latlng (value direction)
  (let* ((p (- (position #\. value) 2))
         (degrees (parse-integer (subseq value 0 p)))
         (minutes (read-from-string (subseq value p)))
         (latlng (+ degrees (/ minutes 60)))
         (sign (cond 
                 ((equal direction "N") 1.0d0)
                 ((equal direction "S") -1.0d0)
                 ((equal direction "E") 1.0d0)
                 ((equal direction "W") -1.0d0)
                 (T (error "Invalid sign ~a" direction)))))
    (* latlng sign)))

(defun close-nmea-socket (routing)
  (when (routing-nmea-socket routing)
    (ignore-errors
     (mbedtls:close-socket (routing-nmea-socket routing)))
    (setf (routing-nmea-socket routing) nil)))

(defun reset-nmea-socket (routing host port)
  (when (routing-nmea-socket routing)
    (log2:info "Closing NMEA socket")
    (close-nmea-socket routing))
  (log2:info "Connecting to NMEA socket at ~a:~a" host port)
  (setf (routing-nmea-port routing)
        port)
  (setf (routing-nmea-socket routing)
        (mbedtls:connect host :port port)))

(defun get-nmea-messages (routing host port &key (timeout 250))
  (when (and (routing-nmea-socket routing)
             (not (equal port (routing-nmea-port routing))))
    (log2:info "Closing NMEA socket")
    (close-nmea-socket routing))
  (unless (routing-nmea-socket routing)
    (log2:info "Connecting to NMEA socket at ~a:~a" host port)
    (setf (routing-nmea-port routing)
          port)
    (setf (routing-nmea-socket routing)
          (mbedtls:connect host :port port)))
  (loop
    :for line = (ignore-errors
                 (mbedtls:get-line (routing-nmea-socket routing)
                                   :timeout timeout))
    :while line
    :collect line))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
