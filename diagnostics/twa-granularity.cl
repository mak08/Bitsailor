;;; Isolated 5-degree / 1-degree / bounded-integer endpoint-shooting comparison.
;;; Run in a disposable SBCL process from the Bitsailor directory, after Quicklisp.
(ql:quickload "bitsailor")
(load "tests/gate-search-tests.cl")
(load "tests/fine-tune-tests.cl")
(in-package :bitsailor)
;;; Hold two TWAs on the selected grid and solve their total duration and split
;;; against the same endpoint. Keep the fastest land/corridor-valid plan.
(defun compare-grid-window (routing reference resolution)
  (let* ((controls (fine-controls reference))
         (bounds (fine-angle-bounds controls))
         (time (trackpoint-time (first reference)))
         (pos (trackpoint-position (first reference)))
         (target (trackpoint-position (car (last reference))))
         (duration (timestamp-difference (trackpoint-time (car (last reference))) time))
         (best nil))
    (loop for a from (* resolution (ceiling (car bounds) resolution)) to (cdr bounds) by resolution do
      (loop for b from (+ a resolution) to (cdr bounds) by resolution do
        (dolist (pair (list (list a b) (list b a)))
          (let* ((first-angle (first pair)) (second-angle (second pair))
                 (candidate (fine-shoot routing pos time target
                    (lambda (v)
                      (list (cons first-angle (* (first v) (second v)))
                            (cons second-angle (* (first v) (- 1d0 (second v))))))
                    (list (* duration 0.999d0) 0.5d0)
                    (list (cons (* duration 0.8d0) duration) (cons 0.001d0 0.999d0))
                    '(-0.1d0 0.001d0))))
            (when (and (fine-valid-window-p reference candidate)
                       (or (null best) (timestamp< (trackpoint-time (car (last candidate)))
                                                  (trackpoint-time (car (last best))))))
              (setf best candidate))))))
    best))
;;; Print measured duration and actual controls, without rounding them before replay.
(defun print-grid-result (label start track)
  (format t "~&GRID-RESULT ~a duration=~,6fs controls=~s endpoint=~s~%" label
    (and track (timestamp-difference (trackpoint-time (car (last track))) start))
    (when track (mapcar #'twa-segment-twa (fine-segments track)))
    (when track (trackpoint-position (car (last track))))))
(call-with-fine-test
 (lambda (routing)
   (let* ((*fine-tune-endpoint-tolerance* 0.001d0)
          (*fine-tune-max-evaluations* 30000)
          (*fine-tune-evaluations* 0)
          (*fine-tune-max-seconds* 120d0)
          (info (fine-test-route routing (loop repeat 8 append (list (cons 105d0 120d0) (cons 110d0 120d0)))))
          (reference (routeinfo-best info))
          (time (trackpoint-time (first reference)))
          (pos (trackpoint-position (first reference))))
     (print-grid-result "reference" time reference)
     (print-grid-result "grid-5" time (compare-grid-window routing reference 5d0))
     (print-grid-result "grid-1" time (compare-grid-window routing reference 1d0))
     (print-grid-result "current-integer" time (fine-optimize-window routing reference pos t)))))
