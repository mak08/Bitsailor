;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-04-27 21:33:15>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defun stop-nmea-listener (nc host port)
  (declare (ignore host port))
  (bordeaux-threads:destroy-thread (nmea-connection-listener nc))
  (setf (nmea-connection-listener nc) nil)
  (close-nmea-socket nc))

(defun reset-nmea-listener (nc host port)
  ;; Reconnect socket, start listener thread if not running
  (reset-nmea-socket nc host port)
  (when (nmea-connection-listener nc)
    (ignore-errors
     (bordeaux-threads:destroy-thread (nmea-connection-listener nc))))    
  (setf (nmea-connection-listener nc)
        (start-nmea-listener nc)))

(defun get-nmea-position (nc host port)
  (declare (ignore host port))
  (nmea-connection-cache nc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal functions

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

(defun close-nmea-socket (nc)
  (when (nmea-connection-socket nc)
    (ignore-errors
     (mbedtls:close-socket (nmea-connection-socket nc)))
    (setf (nmea-connection-socket nc) nil)))

(defun reset-nmea-socket (nc host port)
  ;; Close socket and open with (new or unchanged) host and port
  (when (nmea-connection-socket nc)
    (log2:info "Closing NMEA socket")
    (close-nmea-socket nc))
  (log2:info "Connecting to NMEA socket at ~a:~a" host port)
  (setf (nmea-connection-port nc)
        port)
  (setf (nmea-connection-socket nc)
        (mbedtls:connect host :port port)))


(defun start-nmea-listener (nc)
  ;; Start a message fetching thread with current settings
  (let ((listener
          (bordeaux-threads:make-thread
           (lambda ()
             (nmea-listener nc))
           :name (format nil "~a:~a"
                         (nmea-connection-host nc)
                         (nmea-connection-port nc)))))
    (log2:info "Created thread ~a" listener)
    (setf (nmea-connection-listener nc) listener)))

(defun nmea-listener (nc)
  (loop
    :with k = 0
    :do
       (let* ((messages (reverse
                         (get-nmea-messages nc
                                            (nmea-connection-host nc)
                                            (nmea-connection-port nc))))
              (gprmc (loop
                       :for m :in messages 
                       :when (search "$GPRMC" m)
                         :do (progn
                               (log2:info "~a" m)
                               (return (parse-gprmc m))))))
         (cond
           ((null gprmc)
            (sleep 10))
           (t
            (setf (nmea-connection-cache nc) gprmc)
            (sleep 60))))))

(defun get-nmea-messages (nc host port &key (timeout 250))
  (when (and (nmea-connection-socket nc)
             (or (not (equal host (nmea-connection-host nc)))
                 (not (equal port (nmea-connection-port nc)))))
    (log2:info "Closing NMEA socket")
    (close-nmea-socket nc))
  (unless (nmea-connection-socket nc)
    (log2:info "Connecting to NMEA socket at ~a:~a" host port)
    (setf (nmea-connection-host nc)
          host)
    (setf (nmea-connection-port nc)
          port)
    (setf (nmea-connection-socket nc)
          (mbedtls:connect host :port port)))
  (loop
    :for line = (ignore-errors
                 (mbedtls:get-line (nmea-connection-socket nc)
                                   :timeout timeout))
    :while line
    :collect line))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
