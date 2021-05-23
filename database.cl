;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-05-23 15:12:41>

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

(defparameter +int+             "int")
(defparameter +float+           "float")
(defparameter +timestamp+       "int")
(defparameter +id20+            "char(20)")     ;; XXXXXX-XXXXXX-XXXXXX
(defparameter +uuid+            "char(36)")
(defparameter +tag+             "varchar(16)")
(defparameter +shortstring+     "varchar(40)")
(defparameter +mediumstring+    "varchar(160)")
(defparameter +longstring+      "varchar(320)")
(defparameter +text+            "text")

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
  (:table "user"
   :columns (("email" :datatype +mediumstring+)
             ("boatname" :datatype +mediumstring+)
             ("pwhash" :datatype +shortstring+)
             ("status" :datatype +tag+))
   :constraints ((:primary-key "pk_user" :columns ("email"))
                 (:unique-key "pk_boat" :columns ("boatname"))))
  (:table "user_prov"
   :columns (("email" :datatype +mediumstring+)
             ("boatname" :datatype +mediumstring+)
             ("pwhash" :datatype +shortstring+)
             ("status" :datatype +tag+)
             ("secret" :datatype +uuid+))
   :constraints ((:primary-key "pk_user" :columns ("email"))
                 (:unique-key "pk_boat" :columns ("boatname"))))
  (:table "routing"
   :columns (("user_id" :datatype +mediumstring+)
             ("race_id" :datatype +mediumstring+)
             ("startdate" :datatype +shortstring+)
             ("duration" :datatype +int+)
             ("polars" :datatype +shortstring+)
             ("options" :datatype +mediumstring+)
             ("start_lat" :datatype +float+)
             ("start_lon" :datatype +float+)
             ("dest_lat" :datatype +float+)
             ("dest_lon" :datatype +float+)
             ("nmea_port" :datatype +shortstring+))
   :constraints ((:primary-key "pk_routing" :columns ("user_id" "race_id")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create datatypes from schema

(eval-when (:load-toplevel)
  (edm:use-schema "virtualhelm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defun get-users ()
  (sql:tuples
     (sql:?select '* :from 'virtualhelm.user :into 'virtualhelm.user)))

(defun get-user-by-secret (secret)
  (get-user-by-column 'secret secret))

(defun get-user-by-email (email)
  (get-user-by-column 'email email :case-sensitive nil))

(defun get-user-by-boatname (boatname)
  (get-user-by-column 'boatname boatname :case-sensitive nil))

(defun get-user-prov-by-boatname (boatname)
  (get-user-prov-by-column 'boatname boatname :case-sensitive nil))

(defun get-user-prov-by-secret (secret)
  (get-user-prov-by-column 'secret secret))

(defun get-user-by-column (column value &key (case-sensitive t))
  (let* ((colval
           (if case-sensitive
               column
               (sql:?upper column)))
         (compval
           (if case-sensitive
               value 
               (string-upcase value)))
         (result
           (sql:tuples
            (sql:?select '* :from 'virtualhelm.user
                            :into 'virtualhelm.user
                            :where (sql:?= colval compval)))))
    (when (= (length result) 1)
      (aref result 0))))

(defun get-user-prov-by-column (column value)
  (let ((result
          (sql:tuples
           (sql:?select '* :from 'virtualhelm.user_prov
                           :into 'virtualhelm.user_prov
                           :where (sql:?= column value)))))
    (when (= (length result) 1)
      (aref result 0))))

(defun add-user-provisional (email pwhash boatname status activation-secret)
  (sql:?upsert (make-instance 'virtualhelm.user_prov
                              :email email
                              :pwhash (string-upcase pwhash)
                              :boatname boatname
                              :status status
                              :secret activation-secret)))

(defun add-user (email pwhash boatname status)
  (sql:?upsert (make-instance 'virtualhelm.user
                              :email email
                              :pwhash (string-upcase pwhash)
                              :boatname boatname
                              :status status)))

(defun vh-authorizer (handler request)
  (or
   (authorize-user request)
   (default-authorizer handler request)))

(defun vh-function-authorizer (handler request function)
  (or
   (authorize-user request)
   (function-authorizer handler request function)))

(defun authorize-user (request)
  (sqlite-client:with-current-connection (c *db*)
    (multiple-value-bind (username password)
        (http-credentials request)
      (let ((user (get-user-by-boatname username)))
        (and user
             (string= (status user) "active")
             (string= (pwhash user) (md5 password)))))))

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
  (mbedtls:mbedtls-md message :method "SHA256" :result-type :chars))

(defun md5 (message)
  (mbedtls:mbedtls-md message :method "MD5" :result-type :chars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers - creating and resetting the database

(defun deploy (&key (force nil))
   (sqlite-client:with-current-connection (c *db* :if-does-not-exist :create)
     (sql:create-schema (sql:get-schema-by-name "virtualhelm")))
   (values t))

(defun drop (&key (force nil))
   (sqlite-client:with-current-connection (c *db* :if-does-not-exist :create)
     (sql:%drop-schema "virtualhelm")))

(defun clear ()
  (sqlite-client:with-current-connection (c *db*)
    (sql:sql-exec c "PRAGMA foreign_keys = OFF;")
    (sql::clear-schema (sql:get-schema-by-name "virtualhelm"))
    (sql:sql-exec c "PRAGMA foreign_keys = ON;")))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
