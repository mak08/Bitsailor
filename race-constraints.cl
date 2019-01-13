;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-01-12 00:36:39>

(in-package :virtualhelm)

(remove-constraints)

(add-constraint "393.1"
                (make-instance 'eastbound-south-gate
                               :latitude -11.006d0
                               :longitude 122.8683d0))
(add-constraint "393.1"
                (make-instance 'eastbound-north-gate
                               :latitude -6.6564d0
                               :longitude 131.5906d0))


(add-constraint "393.1"
                (make-instance 'limit-south
                               :latitude -30.0))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
