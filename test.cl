;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2022-04-24 22:47:32>

(in-package :virtualhelm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TWA - heading

(loop :for twd :from 0d0 to 360d0 :by 10d0 :do
  (check-equal (heading-twa twd (twa-heading twd 20d0))
               20d0))

(loop :for twd :from 0d0 to 360d0 :by 10d0 :do
  (check-equal (heading-twa twd (twa-heading twd 90d0))
               90d0))

(loop :for twd :from 0d0 to 360d0 :by 10d0 :do
  (check-equal (heading-twa twd (twa-heading twd 120d0))
               120d0))

(loop :for twd :from 0d0 to 360d0 :by 10d0 :do
  (check-equal (heading-twa twd (twa-heading twd 180d0))
               180d0))

(loop :for twd :from 0d0 to 360d0 :by 10d0 :do
  (check-equal (heading-twa twd (twa-heading twd -90d0))
               -90d0))

(loop :for twa :from 0d0 to 180d0 :by 10d0 :do
  (check-equal (heading-twa 10d0 (twa-heading 10d0 twa))
               twa))

(loop :for twa :from 0d0 to 180d0 :by 10d0 :do
  (check-equal (heading-twa 120d0 (twa-heading 120d0 twa))
               twa))

(loop :for twa :from 0d0 to 180d0 :by 10d0 :do
  (check-equal (heading-twa 245d0 (twa-heading 245d0 twa))
               twa))

(loop :for twa :from 0d0 to 180d0 :by 10d0 :do
  (check-equal (heading-twa 270d0 (twa-heading 270d0 twa))
               twa))

(loop :for twa :from 0d0 to 180d0 :by 10d0 :do
  (check-equal (heading-twa 345d0 (twa-heading 345d0 twa))
               twa))

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

 
