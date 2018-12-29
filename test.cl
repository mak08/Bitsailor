;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-12-29 17:39:47>

(in-package :virtualhelm)

(eval-when (:load-toplevel :execute)
  (defmacro check-delta-equal (form result)
    `(progn
       (assert (equal (ftruncate ,form 0.00001) (ftruncate ,result 0.00001)))
       t)
    )

  (defmacro check-equal (form result)
    `(progn
       (assert (equal ,form ,result))
       t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

