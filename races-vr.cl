;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2024-09-22 21:14:43>

(in-package :bitsailor)

(defmethod store-race-data-vr ((json-array vector))
  (loop
    :for entry :across json-array
    :do (let* ((body-string (joref entry "body"))
               (body-json (parse-json (remove #\\ body-string)))
               (leg (joref (joref body-json "res") "leg"))
               (id (joref leg "_id"))
               (race-id (joref id "race_id"))
               (leg-num (or (joref id "num") (joref id "leg_num")))
               (leg-id (format nil "~a.~a" race-id leg-num)))
          (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
          (setf (gethash leg-id *races-ht*)
                (make-race-info-vr :data leg)))))

(defmethod store-race-data-vr ((races list))
  (loop
    :for entry :in races
    :do (let* ((data (parse-json entry))
               (race (joref data "scriptData"))
               (leg (joref race "leg"))
               (id (joref leg "_id"))
               (race-id (joref id "race_id"))
               (leg-num (or (joref id "num") (joref id "leg_num")))
               (leg-id (format nil "~a.~a" race-id leg-num)))
          (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
          (setf (gethash leg-id *races-ht*)
                (make-race-info-vr :data leg)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
