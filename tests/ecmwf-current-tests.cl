;;; Load after Bitsailor and gate-search-tests.cl; call TEST-ECMWF-CURRENTS.
(in-package :bitsailor)

(defun test-ecmwf-currents ()
  "Check ocean parameter selection, paths, schedules and model-dependent physics."
  (let* ((time (parse-datetime "2026-11-15T12:00:00Z"))
         (cycle (cl-weather::make-cycle% :timestamp time))
         (source (cl-weather::get-datasource 'cl-weather:ecmwf-ifs-current cycle))
         (wind (cl-weather::get-datasource 'cl-weather::ecmwf-ifs-wind cycle))
         (switch (routing-time+ time 86400))
         (routing (make-routing :currents 'cl-weather::fw-current-english-channel
                    :forecast-model 'cl-weather:noaa-gfs-wind
                    :secondary-forecast-model 'cl-weather::ecmwf-aifs-wind
                    :forecast-switch-time switch)))
    (assert (equal "current" (cl-weather::datakind source)))
    (assert (equal '("sve" "svn") (cl-weather::uv-variables source)))
    (assert (not (typep source 'cl-weather::datakind-wind)))
    (assert (equal (cl-weather::data-uri wind 3) (cl-weather::data-uri source 3)))
    (assert (equal (cl-weather::index-uri wind 3) (cl-weather::index-uri source 3)))
    (assert (not (equal (cl-weather::local-pathname wind 3) (cl-weather::local-pathname source 3))))
    (assert (= 360 (cl-weather::maxstep source)))
    (call-with-gate-test-functions
     (list (cons 'cl-weather::retrieve-index-file
                 (lambda (&rest args) (declare (ignore args))
                   (format nil "{\"param\":\"10u\",\"_offset\":0,\"_length\":100}~%{\"param\":\"sve\",\"_offset\":100,\"_length\":200}~%{\"param\":\"svn\",\"_offset\":300,\"_length\":150}~%"))))
     (lambda ()
       (let ((ranges (cl-weather::get-grib-file-ranges source 0)))
         (assert (= 2 (length ranges)))
         (assert (= 100 (cl-weather::range-start (first ranges))))
         (assert (= 449 (cl-weather::range-end (second ranges)))))))
    (assert (= 144 (cl-weather::maxstep
                     (cl-weather::get-datasource 'cl-weather:ecmwf-ifs-current
                       (cl-weather::make-cycle% :timestamp (routing-time+ time 21600))))))
    (assert (eq (routing-current-source routing time) 'cl-weather::fw-current-english-channel))
    (assert (eq (routing-current-source routing switch) 'cl-weather:ecmwf-ifs-current))
    (call-with-gate-test-functions
     (list (cons 'current-display-field
                 (lambda (selected &rest args) (declare (ignore args))
                   (assert (eq selected 'cl-weather:ecmwf-ifs-current))
                   (make-current-field :source selected :vectors #()))))
     (lambda ()
       (let* ((request (make-instance 'polarcl::http-basic))
              (response (make-instance 'polarcl::http-response :request request :status-code 200 :status-text "OK")))
         (|getCurrentField| nil request response :|source| "FW-CURRENT-ENGLISH-CHANNEL"
            :|forecast_model| "ECMWF-AIFS-WIND" :|time| "2026-11-15T20:00:00Z"
            :|north| "50" :|south| "49" :|west| "-3" :|east| "-1" :|columns| "2" :|rows| "2")
         (assert (= 200 (status-code response))))))
    (dolist (model '(cl-weather::ecmwf-ifs-wind cl-weather::ecmwf-aifs-wind))
      (assert (eq (current-source-for-model 'cl-weather::fw-current-agulhas model)
                  'cl-weather:ecmwf-ifs-current)))
    (setf (routing-currents routing) nil)
    (assert (null (routing-current-source routing switch)))
    (assert (null (routing-current-params routing switch)))
    (setf (routing-currents routing) 'cl-weather::fw-current-english-channel)
    (let ((calls nil))
      (call-with-gate-test-functions
       (list (cons 'get-params (lambda (source time &rest args)
                                (declare (ignore time args)) (push source calls) :current)))
       (lambda ()
         (assert (eq :current (routing-current-params routing time)))
         (assert (eq :current (routing-current-params routing switch)))
         (assert (equal '(cl-weather:ecmwf-ifs-current cl-weather::fw-current-english-channel) calls)))))
    ;; Synthetic ECMWF bitmap values must not produce thousand-knot currents.
    (let ((field (cl-weather::make-uv :info (cl-weather::make-gribinfo :missing-value 9999d0)
                   :u-array (make-array 3 :element-type 'double-float :initial-contents '(1d0 9999d0 3d0))
                   :v-array (make-array 3 :element-type 'double-float :initial-contents '(2d0 4d0 9999d0)))))
      (cl-weather::clear-missing-current-vectors field)
      (assert (equalp #(1d0 0d0 0d0) (cl-weather::uv-u-array field)))
      (assert (equalp #(2d0 0d0 0d0) (cl-weather::uv-v-array field)))))
  (format t "~&ECMWF current parameters, schedules, model switches and missing-data tests passed.~%"))
