;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2020-12-15 19:19:02>

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

(let* ((lat0 (dms-to-decimal (make-dms :u -1d0 :d 56  :m 24 :s 23 :cs 0)))
       (lng0 (dms-to-decimal (make-dms :u 1d0 :d 184 :m 55 :s 25 :cs 0)))
       (lat1 (dms-to-decimal (make-dms :u -1d0 :d 56  :m 26 :s 03 :cs 0)))
       (lng1 (dms-to-decimal (make-dms :u 1d0 :d 185 :m 12 :s 08 :cs 0)))
       (p0 (make-latlng :lat lat0 :lng lng0))
       (p1 (make-latlng :lat lat1 :lng lng1))
       (limits (get-race-limits (load-race-definition "/home/michael/Races/RaceInfo_VendeeGlobe_2020-12-10.json"))))
  (check-limits p0 p1 limits))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
