;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2023-03-15 22:02:04>

(in-package :bitsailor)

(defun store-race-data-vr (json-array)
  (loop
    :for entry :across json-array
    :do (let* ((body-string (joref entry "body"))
               (body-json (parse-json (remove #\\ body-string)))
               (leg (joref (joref body-json "res") "leg"))
               (id (joref leg "_id"))
               (race-id (joref id "race_id"))
               (leg-num (joref id "num"))
               (leg-id (format nil "~a.~a" race-id leg-num)))
          (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
          (setf (gethash leg-id *races-ht*)
                (make-race-info-vr :data leg)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
