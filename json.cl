;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;;   A naïve function to convert Lisp data to JSON format.
;;; Author        Michael Kappert 2014
;;; Last Modified  <michael 2017-08-26 00:08:45>
 
(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  - Does support circular objects. An error will be signalled.
;;;  - Does not support all Lisp data types. Empty or invalid JSON will be generated.
;;;  - Not reversible. JSON cannot be mapped back to Lisp except in some trivial cases.

(defun json (stream thing)
  "Serialize a Lisp object to JSON. Does not support circular references."
  (let ((seen-ht (make-hash-table)))
    (declare (special seen-ht))
    (json% stream thing)))

(defgeneric json% (stream thing))

(defmethod json% (stream (thing t))
  (format stream "\"~a\"" thing))
 
(defmethod json% (stream (thing number))
  (format stream "~a" thing))

(defmethod json% (stream (thing double-float))
  (format stream "~6$" thing))

(defmethod json% (stream (thing string))
  (format stream "\"~a\"" thing))
 
(defmethod json% (stream (thing null))
  (format stream "[]"))

(defmethod json% (stream (thing (eql nil)))
  (format stream "false"))
(defmethod json% (stream (thing (eql t)))
  (format stream "true"))
 
(defmethod json% (stream (thing structure-object))
  (declare (special seen-ht))
  (cond
    ((gethash thing seen-ht)
     (error "Circular objects are not supported"))
    (t
     ;; (setf (gethash thing seen-ht) t)
     (format stream "{")
     (loop
        :for (slot . rest) :on (class-slots (class-of thing))
        :do  (progn
               (format stream "\"~(~a~)\": " (slot-definition-name slot))
               (json% stream (slot-value thing (slot-definition-name slot))))
        :when rest :do (format stream ", "))
     (format stream "}"))))

(defmethod json% (stream (thing standard-object))
  (declare (special seen-ht))
  (cond
    ((gethash thing seen-ht)
     (error "Circular objects are not supported"))
    (t
     ;; (setf (gethash thing seen-ht) t)
     (format stream "{")
     (loop
        :for (slot . rest) :on (class-slots (class-of thing))
        :for slot-name = (slot-definition-name slot)
        :unless (member (symbol-name slot-name) '("lock--mode") :test #'string-equal)
        :do  (progn
               (format stream "\"~(~a~)\": " slot-name)
               (json% stream (cond
                               ((slot-boundp thing slot-name)
                                (slot-value thing slot-name))
                               (t
                                 'DEFAULT)))
               (when rest :do (format stream ", "))))
      (format stream "}"))))
 
(defmethod json% (stream (thing list))
  (format stream "[")
  (loop
     :for (element . rest) :on thing
     :do (json% stream element)
     :when rest :do (format stream ", "))
  (format stream "]"))
 
(defmethod json% (stream (thing array))
  (format stream "[")
  (loop
     :for k :below (length thing)
     :for element :across thing
     :do (json% stream element)
     :when (< k (1- (length thing))) :do (format stream ", "))
  (format stream "]"))
 
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
