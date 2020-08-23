;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2020-07-16 23:50:17>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database

(defparameter *root-directory*
  (make-pathname :directory (pathname-directory #.*compile-file-truename*)))

(defparameter *db*
  (namestring (merge-pathnames "local/main.sdb" *root-directory*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL datatype mapping
;;;
;;; ToDo: datatype definition should be provided by the SQL module...

(defparameter +int+        "int")
(defparameter +float+      "float")
(defparameter +smallid+    "char(20)")     ;; XXXXXX-XXXXXX-XXXXXX
(defparameter +uuid+       "char(36)")
(defparameter +smallname+  "varchar(20)")
(defparameter +largename+  "varchar(40)")
(defparameter +string+     "text")
(defparameter +text+       "text")
(defparameter +timestamp+  "int")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data model naming conventions
;;; 1 - Primary key fields: If possible use a single primary field named 'id'.
;;; 2 - Attribute fields don't include the table name
;;; 3 - Reference fields are named <table>_<id>
;;; 4 - Primary keys are named pk_<table>
;;; 5 - Foreign keys are named fk_<table>_<field> (because of (3) <field> includes
;;;     the foreign table name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data model
;;;
;;; - The e-mail address must be unique
;;; - An account may hold multiple boats
;;; - A boat does not have a class, the class is determined by the resp. race
;;;   a boat is entering.
;;;
;;; Quantities, Units and data types
;;; - Timestamp: unix epoch (integer; second accuracy)
;;; - Duration: seconds (integer)
;;; - Distance: meters (float)
;;; - Speed: m/s (float)
;;; - Coordinate:
;;;       DEG in table 'places', 'boat_program', 'race_course', 'race_edition'
;;;       RAD in table 'boat_status'
;;;       (float)
;;; - Direction: DEG is used for TWA and HDG (float).  
;;;
;;; Classes are generated from the schema directly, by USE-SCHEMA

(sql:defschema "virtualhelm"
  (:table "session"
          :columns (("id" :datatype +smallid+)
                    ("user_id" :datatype +largename+)
                    ("race_id"  :datatype +largename+)
                    ("race_def" :datatype +text+))
          :constraints ((:primary-key "pk_session" :columns ("id"))))


  (:table "race_mark"
          :columns (("id" :datatype +smallid+)
                    ("session_id" :datatype +smallid+)
                    ("name" :datatype +text+)
                    ("seqno" :datatype +int+)
                    ("latitude" :datatype +float+)
                    ("longitude" :datatype +float+)
                    ("data" :datatype +text+)
                    )
          :constraints ((:primary-key "pk_race_mark" :columns ("id"))
                        (:foreign-key "fk_race_session_id"
                                      :columns ("session_id")
                                      :referenced-table "session"
                                      :referenced-columns ("id")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defun deploy (&key (force nil))
   (sqlite-client:with-current-connection (c *db* :if-does-not-exist :create)
     (sql:create-schema (sql:get-schema-by-name "virtualhelm")))
   (edm:use-schema "virtualhelm")
   (values t))

(defun drop (&key (force nil))
   (sqlite-client:with-current-connection (c *db* :if-does-not-exist :create)
     (sql:%drop-schema "virtualhelm")))

(defun clear ()
  (clrhash *resource-lock-ht*)
  (sqlite-client:with-current-connection (c *db*)
    (sql-exec c "PRAGMA foreign_keys = OFF;")
    (sql::clear-schema (get-schema-by-name "tinysphere"))
    (sql-exec c "PRAGMA foreign_keys = ON;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unique identifiers

(defun create-id20 ()
  (let* ((chars "0123456789ABCDEFGHIJKLMNOPKRSTUVWXYZ")
         (res (make-array 20 :element-type 'character)))
    (dotimes (k 6) (setf (aref res k)
                         (aref chars (random 36))))
    (setf (aref res 6) #\-)
    (dotimes (k 6) (setf (aref res (+ k 7))
                         (aref chars (random 36))))
    (setf (aref res 13) #\-)
    (dotimes (k 6) (setf (aref res (+ k 14))
                         (aref chars (random 36))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash values


(defun sha256 (message)
  (apply #'concatenate 'string
         (map 'list (lambda (c) (format () "~2,'0x" c))
              (mbedtls-md message :method "SHA256"))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
