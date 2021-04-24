;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2021-04-17 21:30:01>

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eastbound North Gate

;;; Can't pass in west-to-east direction below the gate latitude
(check-equal (meets (make-instance 'eastbound-north-gate
                                   :latitude 5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 11d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 9d0)))
             nil)

;;; Can pass in east-to-west direction below the gate latitude
(check-equal (meets (make-instance 'eastbound-north-gate
                                   :latitude 5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 9d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 11d0)))
             T)

;;; Can pass above gate latitude
(check-equal (meets (make-instance 'eastbound-north-gate
                                   :latitude 5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 6d0) :lngr% (rad 15d0))
                    (make-latlng :latr% (rad 6d0) :lngr% (rad 10d0)))
             T)

;;; You can go east before the gate
(check-equal (meets (make-instance 'eastbound-north-gate
                                   :latitude 5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 9d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 5d0)))
             T)

;;; You can go east after the gate
(check-equal (meets (make-instance 'eastbound-north-gate
                                   :latitude 5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 15d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 11d0)))
             T)

;;; You can go east above and before the gate
(check-equal (meets (make-instance 'eastbound-north-gate
                                   :latitude 5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 6d0) :lngr% (rad 9d0))
                    (make-latlng :latr% (rad 6d0) :lngr% (rad 5d0)))
             T)

;;; You can go east above and after the gate
(check-equal (meets (make-instance 'eastbound-north-gate
                                   :latitude 5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 6d0) :lngr% (rad 15d0))
                    (make-latlng :latr% (rad 6d0) :lngr% (rad 11d0)))
             T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eastbound South Gate

;;; Can't pass in west-to-east direction below the gate latitude
(check-equal (meets (make-instance 'eastbound-south-gate
                                   :latitude -5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 11d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 9d0)))
             nil)

;;; Can pass below gate latitude
(check-equal (meets (make-instance 'eastbound-south-gate
                                   :latitude -5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad -6d0) :lngr% (rad 15d0))
                    (make-latlng :latr% (rad -6d0) :lngr% (rad 10d0)))
             T)

;;; Can pass in east-to-west direction
(check-equal (meets (make-instance 'eastbound-south-gate
                                   :latitude -5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 9d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 11d0)))
             t)

;;; You can go east before the gate
(check-equal (meets (make-instance 'eastbound-south-gate
                                   :latitude -5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 9d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 5d0)))
             T)

;;; You can go east after the gate
(check-equal (meets (make-instance 'eastbound-south-gate
                                   :latitude -5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 15d0))
                    (make-latlng :latr% (rad 0d0) :lngr% (rad 11d0)))
             T)

;;; You can go east below and before the gate
(check-equal (meets (make-instance 'eastbound-south-gate
                                   :latitude -5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad -6d0) :lngr% (rad 9d0))
                    (make-latlng :latr% (rad -6d0) :lngr% (rad 5d0)))
             T)

;;; You can go east below and after the gate
(check-equal (meets (make-instance 'eastbound-south-gate
                                   :latitude -5.0d0
                                   :longitude 10.0d0) 
                    (make-latlng :latr% (rad -6d0) :lngr% (rad 15d0))
                    (make-latlng :latr% (rad -6d0) :lngr% (rad 11d0)))
             T)

;; Pos: 56°24'23"S 184°55'25"E
;; Pos: 56°26'03"S 185°12'08"E


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
