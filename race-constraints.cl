;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-01-24 17:38:59>

(in-package :bitsailor)

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
                (make-instance 'eastbound-south-gate
                               :latitude (deg -0.1810d0)
                               :longitude (deg 2.4846d0)))

(add-constraint "393.1"
                (make-instance 'limit-south
                               :latitude -30.0))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
