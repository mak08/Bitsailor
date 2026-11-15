;;; Load after Bitsailor and gate-search-tests.cl; call TEST-CURRENT-FIELD.
(in-package :bitsailor)

(defun test-current-field ()
  "Verify flow components, viewport sampling, regional limits and endpoint validation."
  (let* ((time (parse-datetime "2026-11-15T20:00:00Z"))
         (grid (cl-weather::make-gribinfo :lat-start 1d0 :lat-end -1d0 :lat-points 3
                  :lon-start 0d0 :lon-end 360d0 :lon-points 361 :i-inc 1d0 :j-inc 1d0))
         (uv (cl-weather::make-uv :cycle time :info grid
               :u-array (make-array 1083 :element-type 'double-float :initial-element 1d0)
               :v-array (make-array 1083 :element-type 'double-float :initial-element 2d0)))
         (params (cl-weather::make-params :fc0 uv :fc1 uv))
         (*use-bitmap* nil)
         (samples 0))
    (call-with-gate-test-functions
     (list (cons 'cl-map:point-on-land-p (lambda (position) (declare (ignore position)) nil))
           (cons 'get-params (lambda (source timestamp &rest args)
                              (declare (ignore args))
                              (assert (eq source 'cl-weather::fw-current-english-channel))
                              (assert (timestamp= timestamp time))
                              (incf samples) params)))
     (lambda ()
       (let ((field (current-display-field 'cl-weather::fw-current-english-channel time
                     0.8d0 0.2d0 -1d0 1d0 3 2)))
         (assert (= samples 1))
         (assert (= 6 (length (current-field-vectors field))))
         (assert (every (lambda (v) (and (= 1d0 (aref v 0)) (= 2d0 (aref v 1))))
                        (current-field-vectors field))))
       ;; Nonzero offshore GRIB values interpolate across a shoreline, but only
       ;; samples whose geographic centres are at sea should produce arrows.
       (call-with-gate-test-functions
        (list (cons 'cl-map:point-on-land-p
                    (lambda (position) (> (latlng-lng position) 0.2d0))))
        (lambda ()
          (let ((vectors (current-field-vectors
                           (current-display-field 'cl-weather::fw-current-english-channel time
                             0.8d0 0.2d0 -1d0 1d0 3 2))))
            (dolist (index '(0 1 3 4)) (assert (equalp #(1d0 2d0) (aref vectors index))))
            (dolist (index '(2 5)) (assert (equalp #(0d0 0d0) (aref vectors index)))))))
       (let ((*use-bitmap* t))
         (call-with-gate-test-functions
          (list (cons 'bm-is-land (lambda (position) (declare (ignore position)) t)))
          (lambda ()
            (assert (equalp #(0d0 0d0) (aref (current-field-vectors
                       (current-display-field 'cl-weather::fw-current-english-channel time
                         0.8d0 0.2d0 -1d0 1d0 1 1)) 0))))))
       (let ((field (current-display-field 'cl-weather::fw-current-english-channel time
                     5d0 4d0 0d0 1d0 1 1)))
         (assert (equalp #(0d0 0d0) (aref (current-field-vectors field) 0))))
       (let* ((request (make-instance 'polarcl::http-basic))
              (response (make-instance 'polarcl::http-response :request request :status-code 200 :status-text "OK"))
              (args (list :|source| "FW-CURRENT-ENGLISH-CHANNEL" :|time| "2026-11-15T20%3A00%3A00.000Z"
                          :|north| "0.8" :|south| "0.2" :|west| "-1" :|east| "1"
                          :|columns| "3" :|rows| "2")))
         (assert (search "\"vectors\": [" (apply #'|getCurrentField| nil request response args)))
         (assert (= 200 (status-code response)))
         (dolist (invalid '((:|source| "NOAA-GFS-WIND") (:|columns| "99999")
                            (:|time| "invalid") (:|north| "90")))
           (let ((test-args (copy-list args)))
             (setf (getf test-args (first invalid)) (second invalid))
             (apply #'|getCurrentField| nil request response test-args)
             (assert (= 400 (status-code response)))))))))
  (format t "~&Current field interpolation, coverage and API validation passed.~%"))
