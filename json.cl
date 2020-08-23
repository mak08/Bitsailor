;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;;   A naïve function to convert Lisp data to JSON format.
;;; Author        Michael Kappert 2014
;;; Last Modified  <michael 2020-07-26 15:12:53>
 
(in-package :virtualhelm)

;; JSON Object
(defstruct json-object fields)

;; JSON Array
;; Represented by Lisp array

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

(defmethod json% (stream (thing timestamp))
  (format stream "\"~a\"" (format-datetime nil thing)))

(defmethod json% (stream (thing duration))
  (format stream "\"~s\"" thing))

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

(defmethod json% (stream (thing json-object))
  (format stream "{")
  (loop
     :for (field . rest) :on (json-object-fields thing)
     :do (progn
           (format stream "\"~a\": " (json-field-name field))
           (json% stream (json-field-value field))
           (when rest (format stream ","))))
  (format stream "}"))
 
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
;;; Special types

(defmethod json% (stream (thing cl-weather::uv))
  (format stream "~a" thing))

(defmethod json% (stream (thing vector))
  (typecase thing
    (latlng
     (format stream "{\"lat\":~5$, \"lng\":~5$}"
             (latlng-lat thing)
             (latlng-lng thing)))
    (t
     (format stream "[")
     (loop
        :for k :below (length thing)
        :for element :across thing
        :do (json% stream element)
        :when (< k (1- (length thing))) :do (format stream ", "))
     (format stream "]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deserializing JSON


(defmethod print-object ((thing json-object) stream)
  (format stream "<~{~a~^, ~}>" (json-object-fields thing)))

(defstruct json-field name value)
(defmethod print-object ((thing json-field) stream)
  (format stream "~a: ~a"
          (json-field-name thing)
          (json-field-value thing)))

(defun joref (json-object field-name)
  (loop
     :for jf :in (json-object-fields json-object)
     :when (string= (json-field-name jf) field-name)
     :return (json-field-value jf)))

(defun parse-json-file (name)
  (let ((filename (merge-pathnames name
                                   (merge-pathnames (make-pathname :type "json")
                                                     (pathname *polars-dir*)))))
    (log2:trace "Loading JSON from ~a~%" filename)
    (with-open-file (f filename :element-type 'character)
      (let ((json-string (make-string (file-length f))))
        (read-sequence json-string f)
        (log2:trace "Parsing, size: ~a" (length json-string))
        (parse-json json-string)))))


(defun parse-json (s)
  (let ((pos 0))
    (declare (special pos))
    (whitespace s)
    (parse-json-expr s)))

(defun parse-json-expr (s)
  (declare (special pos))
  (case (aref s pos)
    (#\{
     (parse-json-object s))
    (#\[
     (parse-json-array s))
    (t
     (cond
       ((peek s #\")
        (parse-json-string s))
       ((starts-with s "true" pos)
        (incf pos 4)
        (whitespace s)
        'true)
       ((starts-with s "false" pos)
        (incf pos 5)
        (whitespace s)
        'false)
       ((starts-with s "null" pos)
        (incf pos 4)
        (whitespace s)
        'null)
       ((is-numchar (aref s pos))
        (parse-json-number s))
       (t
        (error "Invalid character ~a at position ~a" (aref s pos) pos))))))

(defun parse-json-array (s)
  (declare (special pos))
  (expect s #\[)
  (whitespace s)
  (let ((elements
         (when (not (peek s #\]))
           (cons (parse-json-expr s)
                 (loop
                    :while  (peek s #\,)
                    :do (progn (incf pos)
                               (whitespace s))
                    :collect (parse-json-expr s))))))
    (expect s #\])
    (whitespace s)
    (make-array (length elements) :initial-contents elements)))
  
(defun parse-json-object (s)
  (declare (special pos))
  (expect s #\{)
  (whitespace s)
  (let ((fields
         (when (peek s #\")
           (cons  (parse-json-field s)
                  (loop
                     :while (peek s #\,)
                     :do (progn (incf pos)
                                (whitespace s))
                     :collect (parse-json-field s))))))
    (expect s #\})
    (whitespace s)
    (make-json-object :fields fields)))

(defun parse-json-field (s)
  (declare (special pos))
  (let ((name (parse-json-string s)))
    (expect s #\:)
    (whitespace s)
    (let ((value (parse-json-expr s)))
      (whitespace s)
      (make-json-field :name name :value value))))

(defun parse-json-string (s)
  (declare (special pos))
  (expect s #\")
  (let ((end (string-end s :start pos)))
    (when (null end)
      (error "Missing closing string delimiter for string starting at ~a" pos))
    (let ((string (subseq s pos end)))
      (setf pos (1+ end))
      (whitespace s)
      (values string))))

(defun string-end (s &key start (delim #\") (escape #\\))
  (declare (string s)
           (character delim escape))
  (loop
     :for k :from start :below (length s)
     :until (and (eql (aref s k) delim)
                 (not (eql (aref s (1- k)) escape)))
     :finally (return
                (when (< k (length s))
                  k))))

(defun parse-json-number (s)
  (declare (special pos))
  (let ((end
         (loop
            :for end :from pos
            :while (is-numchar (aref s end))
            :finally (return end))))
    (let ((n (read-from-string s nil nil :start pos :end end)))
      (unless (numberp n)
        (error "At position ~: invalid number ~a" pos (subseq s pos end)))
      (setf pos end)
      (whitespace s)
      n)))


(defun peek (s c)
  (declare (special pos))
  (eql (aref s pos) c))

(defun expect (s c)
  (declare (special pos))
  (unless (eql (aref s pos) c)
    (error "At position ~a: expected ~a but got ~a" pos c (aref s pos)))
  (incf pos))

(defun whitespace (s)
  (declare (special pos))
  (loop :while (and (< pos (length s))
                    (is-white (aref s pos))) :do (incf pos)))
  
(defun is-numchar (c)
  (position c "0123456789.Ee+-"))

(defun starts-with (s1 s2 start1)
  (let ((m (mismatch s1 s2 :start1 start1)))
    (or (null m)
        (>= m (+ start1 (length s2))))))

(defun is-white (char)
  (declare (character char))
  (or (eql char #\Space)
      (eql char #\Tab)
      (eql char #\Return)
      (eql char #\Newline)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
