;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2024-07-10 22:53:01>

(in-package :bitsailor)

(defstruct boat-action-event
  :|@class| :|authToken| :|eventKey| :|playerId| :|requestId|
  :|race_id| :|leg_num| :|ts| :|actions|)

(defstruct sailaction |value| (|type| "sail"))

(defstruct progaction |values| (|type| "prog"))
(defstruct wpaction |values| (|type| "wp") |nextWpIdx|)

(defstruct command |ts| |heading| |autoTwa|)
(defstruct waypoint |lat| |lon| |idx|)

(defun minute-next () (* 60000 (ceiling (timestamp-to-unix (now)) 60)))
(defun time-to-millies (|yyyy-mm-ddThh:mm:dd:ss|)
  (* (timestamp-to-unix (parse-timestring |yyyy-mm-ddThh:mm:dd:ss|)) 1000))

(defparameter *sailactions*
  (list (make-sailaction :|value| 3)))

(defun make-commands (&key (n 8) (heading 90))
  (let* ((ts0 (timestamp-minimize-part (now) :sec))
         (min (timestamp-minute ts0))
         (ts
           (adjust-timestamp ts0 (offset :minute (- (+ 20 (* 10 (ceiling min 10))) min)))))
    (list
     (make-progaction
      :|type| "prog"
      :|values| (loop
                  :for k :below n
                  :collect (make-command
                            :|ts| (time-to-millies
                                   (format-datetime nil
                                                    (adjust-timestamp ts (offset :minute (* k 10)))))
                            :|heading| heading
                            :|autoTwa| t))))))


(defun make-waypoints (start-lat start-lon direction)
  (let ((positions
          (loop 
            :with p =  (make-latlng :lat start-lat :lng start-lon)
            :for d :from 1000d0 :to 9000d0 :by 1000d0
            :collect (add-distance-exact p d direction))))
    (list (make-wpaction
           :|nextWpIdx| (1+ (length positions))
           :|values| (loop
                       :for p :in positions
                       :for k :from 1
                       :collect (make-waypoint :|idx| k
                                               :|lat| (coerce (latlng-lat p) 'single-float)
                                               :|lon| (coerce (latlng-lng p) 'single-float)))))))
    

(defun send-commands (actions race-id leg-id
                      &key |device|
                        (|userName| nil)
                        (|password| nil)
                        (|deviceId| "4afaafcb-c1c9-48d4-9212-954c1ffc8801"))
  (let* ((url
           (format nil "https://prod.vro.sparks.virtualregatta.com/rs/device/Xcl3WbCUmfcu5pWCktUoC0slGT4xkbEt/LogEventRequest" |device|))
         (auth-response
           (if (and |userName| |password|)
               (authentication-request :|userName| |userName| :|password| |password|)
               (device-authentication-request :|device| |device| :|deviceId| |deviceId|)))
         (auth-response-body
           (parse-json (curl::http-response-body auth-response)))
         (auth-token
           (joref auth-response-body "authToken"))
         (user-id
           (joref auth-response-body "userId"))
         (body
           (with-output-to-string (s)
             (sleep 2)
             (json s
                   (make-boat-action-event
                    :|@class| "LogEventRequest"
                    :|authToken| auth-token
                    :|eventKey| "Game_AddBoatAction"
                    :|requestId| (format nil "~a_1" (request-timestamp))
                    :|playerId| user-id
                    :|race_id| race-id
                    :|leg_num| leg-id
                    :|ts| (minute-next)
                    :|actions| actions)
                   :preserve-slotname-case t))))
    (curl:http url :method :post :body body)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
{
  "@class": "LogEventRequest",
  "eventKey": "Game_AddBoatAction",
  "race_id": 676,
  "leg_num": 4,
  "ts": 1720276260000,
  "actions": [
    {
      "values": [
        {
          "ts": 1720279800000,
          "heading": 50,
          "autoTwa": true
        },
        {
          "ts": 1720284600000,
          "heading": -45,
          "autoTwa": true
        }
      ],
      "type": "prog"
    }
  ],
  "requestId": "638558802318750000_16",
  "authToken": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VySWQiOiI2NGJlYWQ0ZDIxNDMyZDJmMTU0YTI4OGIiLCJpYXQiOjE3MjAyNzQwMDIsImV4cCI6MTcyMDMxNzIwMiwiYXVkIjoiVmlydHVhbFJlZ2F0dGEiLCJpc3MiOiJhdXRoLnZpcnR1YWxyZWdhdHRhLmNvbSIsInN1YiI6InZyYXV0aCJ9.aqBqqtzzHM3fXHFrwEUrHSmYI20X9EODRFxxJp_0QrcY2asDLmkHDcWwLFed041ygt1t_YdowDhqgv0twmzcrxJ7as2geRqBW0Yo797xvqKtAsEM38UghA21scHRZ1Mo4IF_UtFKlO63936aqS72ML7__9bRq8BpNFHobJXvxx_g5WwtxyVRW2hmCacZOfTtwychtPltt9enhVPQSmNIkdsoH8fYnNgKZMGrc4Zlo89wSX3Nm14xjlTsez4rpBOk_oaYnjMKIgGtw_A4ie8Yonfdt2XR0iKntydrZuu91XzPwgXYVjXR4Gq1-ob-p8HJJCLDErCCba0rDc9x7g96wQ",
  "playerId": "64bead4d21432d2f154a288b"
}
|#
