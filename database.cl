;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-05-06 02:17:37>

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

(sql:defschema "virtualhelm"
  (:table "session"
          :columns (("id" :datatype +uuid+)
                    ("user_id" :datatype +largename+))
          :constraints ((:primary-key "pk_session" :columns ("id"))))

  (:table "routing"
          :columns (("id" :datatype +uuid+)
                    ("session_id" :datatype +uuid+)
                    ("race_id" :datatype +text+)
                    ("startdate" :datatype +text+)
                    ("start_lat" :datatype +float+)
                    ("start_lon" :datatype +float+)
                    ("dest_lat" :datatype +float+)
                    ("dest_lon" :datatype +float+)
                    ("nmea_port" :datatype ++)
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
