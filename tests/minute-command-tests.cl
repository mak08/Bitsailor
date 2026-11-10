;;; Load after gate-search-tests.cl and fine-tune-tests.cl.
(in-package :bitsailor)

;;; Neighboring minute schedules must be re-sailed, not just relabelled, and
;;; still meet the original geographic/ETA constraints. Arrival is not a command.
(defun test-minute-commands ()
  (call-with-fine-test
   (lambda (routing)
     (let* ((start (routing-starttime routing))
            (pos (routing-start routing))
            (continuous (fine-replay routing pos start '((105d0 . 90.25d0) (110d0 . 149.75d0))))
            (arrival-only (fine-replay routing pos start '((105d0 . 120.25d0))))
            (candidates (fine-minute-candidates routing continuous)))
       (assert (not (fine-minute-plan-p continuous)))
       (assert (fine-minute-plan-p arrival-only))
       (assert (= 2 (length candidates)))
       (dolist (candidate candidates)
         (assert (fine-minute-plan-p candidate))
         (assert (timestamp= (trackpoint-time (car (last continuous))) (trackpoint-time (car (last candidate)))))
         (assert (> (fine-distance (trackpoint-position (car (last continuous)))
                                   (trackpoint-position (car (last candidate)))) 1d0))
         ;; Moving the switch changed the endpoint: relabelling is not acceptance.
         (assert (not (fine-valid-window-p continuous candidate))))
       (let* ((*fine-tune-enabled* nil)
              (info (make-routeinfo :status "reached" :best continuous
                      :stats (get-statistics continuous 0d0 1 1))))
         (fine-tune-route routing info)
         (assert (not (fine-tuning-minute-controls (routeinfo-fine-tuning info))))
         (assert (null (routeinfo-steering-segments info)))))))
  (let* ((start (local-time:parse-timestring "2026-11-04T20:09:17.500Z"))
         (stepper (make-hires-stepper start 86400)))
    (multiple-value-bind (dt next) (funcall stepper)
      (assert (= dt 42.5d0))
      (assert (routing-minute-p next)))
    (dotimes (i 500)
      (multiple-value-bind (dt next) (funcall stepper)
        (assert (plusp dt)) (assert (routing-minute-p next)))))
  (format t "~&Minute-command replay, unsafe rounding rejection, arrival events and fractional alignment passed.~%"))
