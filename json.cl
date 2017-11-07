;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;;   A naïve function to convert Lisp data to JSON format.
;;; Author        Michael Kappert 2014
;;; Last Modified  <michael 2017-11-03 21:02:11>
 
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
  (format stream "~4$" thing))

(defmethod json% (stream (thing single-float))
  (format stream "~4$" thing))

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
        :for (slotname . rest) :on (filtered-slots thing)
        :do  (progn
               (format stream "\"~(~a~)\": " slotname)
               (json% stream (slot-value thing slotname)))
        :when rest :do (format stream ", "))
     (format stream "}"))))

(defun filtered-slots (thing)
  (let ((slots (class-slots (class-of thing))))
    (loop
       :for slot :in slots
       :for sd-name = (slot-definition-name slot)
       :for sd-name-str = (symbol-name sd-name)
       :unless (eql (aref sd-name-str (1- (length sd-name-str))) #\%)
       :collect sd-name)))
    

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deserializing JSON

(defstruct json-object fields)
(defstruct json-field name value)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun joref (json-object field-name)
  (loop
     :for jf :in (json-object-fields json-object)
     :when (string= (json-field-name jf) field-name)
     :return (json-field-value jf)))
                    
(defun _json (name tree &rest args)
  (etypecase tree
    (rdparse::token
     (read-from-string (token-value tree)))
    (atom
     tree)))

(defun _object (name tree &rest args)
  (make-json-object :fields (cadr tree)))

(defun _array (name tree &rest args)
  (let ((elements (cadr tree)))
    (make-array (length elements) :initial-contents elements)))

(defun _jsonseq (name tree &rest args)
  (typecase tree
    (rdparse::token 
     (list (read-from-string (token-value tree))))
    (atom
     (list tree))
    (otherwise
     (cons (car tree) (caddr tree)))))

(defun _fieldseq (name tree &rest args)
  (if (atom tree)
      (list tree)
      (cons (car tree) (caddr tree))))

(defun _fieldassign (name tree &rest args)
  (make-json-field :name (read-from-string (token-value (car tree))) :value (caddr tree)))
)

(defparser parse-json
    :tokens ((_string (:alt :sq-string :dq-string))
             (_number (:seq (:opt "-") :numeric (:opt (:seq "." :numeric)))))
    :rules ((_json
             (:alt _number _string _object _array))
            (_object
             (:seq "{" _fieldseq "}"))
            (_fieldseq
             (:alt (:seq _fieldassign "," _fieldseq)
                   _fieldassign))
            (_fieldassign
             (:seq :dq-string ":" _json))
            (_array
             (:seq "[" _jsonseq "]"))
            (_jsonseq
             (:alt (:seq _json "," _jsonseq)
                   _json))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
