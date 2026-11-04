;;; Load after gate-search-tests.cl and fine-tune-tests.cl.
(in-package :bitsailor)

;;; Independent switch times in A/B/A let a compressed prefix's position error
;;; cancel in later segments while preserving the original rejoin position/time.
(defun test-fine-balance ()
  (call-with-fine-test
   (lambda (routing)
     (let* ((info (fine-test-route routing
                    (loop for (angle . duration) in '((105d0 . 120d0) (110d0 . 240d0)
                                                      (105d0 . 120d0) (110d0 . 240d0)
                                                      (105d0 . 720d0) (110d0 . 2160d0))
                          append (loop repeat (round duration 120d0) collect (cons angle 120d0)))))
            (raw (routeinfo-best info))
            (candidate (fine-optimize-window routing raw (routing-start routing) nil)))
       (format t "~&Balanced correction: changes ~a -> ~a~%" (fine-changes raw) (and candidate (fine-changes candidate)))
       (assert candidate)
       (assert (<= (fine-changes candidate) 2))
       (assert (timestamp= (trackpoint-time (car (last candidate))) (trackpoint-time (car (last raw)))))
       (assert (fine-valid-window-p raw candidate))
       (assert (fine-integer-plan-p candidate))
       (assert (< (fine-distance (trackpoint-position (car (last raw)))
                                 (trackpoint-position (car (last candidate)))) 0.02d0))
       ;; The intermediate residual is real and intentionally not snapped away.
       (assert (some (lambda (p) (> (fine-distance (fine-reference-position raw (trackpoint-time p))
                                                  (trackpoint-position p)) 1d0)) candidate))))
   t)
  ;; Even baseline heading replay or a large configured step cannot skip a
  ;; minute of wind/VMG validation. The endpoint is freshly evaluated as well.
  (call-with-fine-test
   (lambda (routing)
     (let ((start (routing-starttime routing)))
       (dolist (step '(nil 600d0))
         (let ((track (fine-replay routing (routing-start routing) start
                                  '(((:heading 110) . 1200d0)) :step step)))
           (assert track)
           (loop for (a b) on track while b do
             (assert (<= (timestamp-difference (trackpoint-time b) (trackpoint-time a)) 60d0)))
           (assert (< (abs (- (trackpoint-twa (car (last track))) 98d0)) 1d-6))
           (assert (timestamp= (trackpoint-wind-time (car (last track))) (fine-time+ start 1200d0)))))
       ;; HDG 50 is still within VMG bounds at t=480 (TWA 45.2),
       ;; but the terminal t=540 sample is invalid (TWA 44.6).
       (assert (null (fine-replay routing (routing-start routing) start
                                 '(((:heading 50) . 540d0)) :step nil)))
       (assert (null (fine-replay routing (routing-start routing) start
                                 '(((:heading 50) . 540d0) (80d0 . 60d0)) :step nil)))))
   t)
  (format t "~&Balanced endpoint corrections and one-minute/terminal heading checks passed.~%"))
