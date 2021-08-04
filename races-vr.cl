;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-07-28 17:48:51>

(in-package :virtualhelm)

(defun store-race-data-vr (json-object)
  (loop
    :for race-def :across (joref (joref json-object "scriptData") "res")
    :do (let* ((race-id (joref race-def "raceId"))
               (leg-num (joref race-def "legNum"))
               (leg-id (format nil "~a.~a" race-id leg-num)))
          (log2:info "Loading race ~a ~a" leg-id (joref race-def "legName"))
          (setf (gethash leg-id *races-ht*)
                (make-race-info-vr :data race-def)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
