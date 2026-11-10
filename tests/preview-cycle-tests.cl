;;; Load after Bitsailor and gate-search-tests.cl.
(in-package :bitsailor)

;;; Live preview must not advertise a nominal/partial run. Exact-cycle metadata
;;; remains scheduled metadata and is not changed by this readiness selection.
(defun test-preview-cycles ()
  (let* ((name 'cl-weather:noaa-gfs-wind)
         (current (parse-routing-cycle "2026-11-07T18:00:00Z" name))
         (previous (cl-weather::previous-cycle name current))
         (downloaded 0) (older-available t))
    (flet ((info (source cycle)
             (cl-weather::make-datasource-info :name source :datakind :wind
               :gribpaths (list (cl-weather::make-grib-spec :date (cl-weather::cycle-datestring cycle)
                                  :run (cl-weather::cycle-run cycle) :step 0 :path "test.grib2")))))
      (let ((nominal (info name current)))
        (call-with-gate-test-functions
         (list (cons 'cl-weather:datasource-info (lambda () (list nominal)))
               (cons 'cl-weather:current-cycle (lambda (source) (assert (eq source name)) current))
               (cons 'cl-weather::datasource-info-for-cycle #'info)
               (cons 'cycle-download-count
                     (lambda (sources cycle)
                       (assert (equal sources (list name)))
                       (values (cond ((timestamp= (cl-weather::cycle-timestamp cycle)
                                                  (cl-weather::cycle-timestamp current)) downloaded)
                                     ((and older-available
                                           (timestamp= (cl-weather::cycle-timestamp cycle)
                                                       (cl-weather::cycle-timestamp previous))) 2)
                                     (t 0)) 2))))
         (lambda ()
           (dolist (count '(0 1 2))
             (setf downloaded count)
             (let* ((source (first (server-settings-datasources (get-server-settings))))
                    (path (first (cl-weather::datasource-info-gribpaths source))))
               (assert (= (cl-weather::grib-spec-run path) (if (= count 2) 18 12)))))
           (setf downloaded 0 older-available nil)
           (let ((source (first (server-settings-datasources (get-server-settings)))))
             (assert (eq name (cl-weather::datasource-info-name source)))
             (assert (null (cl-weather::datasource-info-gribpaths source))))
           ;; An explicitly requested run is exact metadata, even when unavailable.
           (assert (search "test.grib2"
                     (|getServerSettings| nil nil nil :|forecast_model| "NOAA-GFS-WIND"
                       :|cycle| "2026-11-07T18:00:00Z")))
           (assert (cl-weather::datasource-info-gribpaths nominal)))))))
  (format t "~&Nominal, partial, complete and missing preview-cycle availability tests passed.~%"))
