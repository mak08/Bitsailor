;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-07-28 17:48:17>

(in-package :virtualhelm)

(defun store-race-data-rs (json-object)
  (loop
    :for race-def :across (joref json-object "results")
    :do
       (let* ((race-id (joref race-def "objectId"))
              (race-name (joref race-def "name")))
         (log2:info "Loading race ~a ~a" race-id race-name)
         (setf (gethash race-id *races-ht*)
               (make-race-info-rs :data race-def)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
