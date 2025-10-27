;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2025-10-26 21:32:14>

(in-package :router)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defun get-gprmc (host port &key (timeout 250) (connect-timeout 1))
  (log2:info "Connecting to NMEA socket at ~a:~a" host port)
  (handler-case
      (let ((socket (sb-ext:with-timeout connect-timeout
                      (mbedtls:connect host :port port))))
        (log2:info "NMEA connection ~a opened" socket)
        (let* ((messages
                 (loop
                   :for line = (ignore-errors
                                (mbedtls:get-line socket :timeout timeout))
                   :while line
                   :collect line)))
          (loop
            :for m :in messages 
            :when (search "$GPRMC" m)
              :do (progn
                    (log2:info "Closing socket ~a" socket)
                    (mbedtls:close-socket socket)
                    (log2:info "~a" m)
                    (return (parse-gprmc m))))))
    (sb-ext:timeout (e)
      nil)))

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

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
