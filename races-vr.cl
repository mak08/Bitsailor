;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-07-11 01:07:56>

(in-package :virtualhelm)

;; caution - get-leg-info also exists
(defun store-race-data-vr (json-object)
     (let* ((leg (joref (joref json-object "res") "leg"))
            (race-id (joref (joref leg  "_id") "race_id"))
            (leg-num (joref (joref leg "_id") "num"))
            (leg-id (format nil "~a.~a" race-id leg-num)))
       (log2:info "Loading race ~a ~a" leg-id (joref leg "name"))
       (setf (gethash leg-id *races-ht*) leg)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
