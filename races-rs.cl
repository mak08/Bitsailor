;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2023-03-15 22:01:55>

(in-package :bitsailor)

(defun store-race-data-rs (json-object)
  (loop
    :for race-def :across (joref json-object "results")
    :do
       (let* ((race-id (joref race-def "objectId"))
              (race-name (joref race-def "name"))
              (norouter-p (eq (joref race-def "norouter") 'true)))
         (cond
           (norouter-p
            (log2:info "Skipping norouter race ~a ~a" race-id race-name))
           (t
            (log2:info "Loading race ~a ~a" race-id race-name)
            (setf (gethash race-id *races-ht*)
                  (make-race-info-rs :data race-def)))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
