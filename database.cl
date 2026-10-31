;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2024-05-21 22:04:23>

(in-package :bitsailor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database

(defparameter *root-directory*
  (make-pathname :directory (pathname-directory #.*compile-file-truename*)))

(defparameter *db*
  (namestring (merge-pathnames "local/main.sdb" *root-directory*)))

(defvar *dbcon*
  )

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

(sql:defschema "bitsailor"
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
  (edm:use-schema "bitsailor"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;; Look up exactly one USER row by COLUMN, optionally ignoring case; return NIL otherwise.
(defun get-user-by-column (column value &key (case-sensitive t))
  (get-by-column 'bitsailor.user column value :case-sensitive case-sensitive))

;;; Look up exactly one USER_PROV row by COLUMN, optionally ignoring case; return NIL otherwise.
(defun get-user-prov-by-column (column value &key (case-sensitive t))
  (get-by-column 'bitsailor.user_prov column value :case-sensitive case-sensitive))

;;; Return all USER rows as schema-backed objects through the current SQL connection.
(defun get-users ()
  (sql:tuples
     (sql:?select '* :from 'bitsailor.user :into 'bitsailor.user)))

;;; Legacy USER lookup by SECRET; the current USER schema has no SECRET column (USER_PROV does).
(defun get-user-by-secret (secret)
  (get-user-by-column 'secret secret))

;;; Look up an account by email, ignoring case.
(defun get-user-by-email (email)
  (get-user-by-column 'email email :case-sensitive nil))

;;; Look up an account by boat name, ignoring case.
(defun get-user-by-boatname (boatname)
  (get-user-by-column 'boatname boatname :case-sensitive nil))

;;; Look up a provisional account by boat name, ignoring case.
(defun get-user-prov-by-boatname (boatname)
  (get-user-prov-by-column 'boatname boatname :case-sensitive nil))

;;; Look up the provisional account matching an activation secret.
(defun get-user-prov-by-secret (secret)
  (get-user-prov-by-column 'secret secret))

;;; Select schema-backed rows matching COLUMN and return the row only when exactly one matches.
(defun get-by-column (table column value &key (case-sensitive t))
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
            (sql:?select '* :from table
                            :into table
                            :where (sql:?= colval compval)))))
    (when (= (length result) 1)
      (aref result 0))))


;;; Upsert USER_PROV with lowercased email, uppercased password hash and activation secret.
(defun add-user-provisional (email pwhash boatname status activation-secret)
  (sql:?upsert (make-instance 'bitsailor.user_prov
                              :email (string-downcase email)
                              :pwhash (string-upcase pwhash)
                              :boatname boatname
                              :status status
                              :secret activation-secret)))

;;; Upsert USER with normalized email and password hash; activation data stays in USER_PROV.
(defun add-user (email pwhash boatname status)
  (sql:?upsert (make-instance 'bitsailor.user
                              :email (string-downcase email)
                              :pwhash (string-upcase pwhash)
                              :boatname boatname
                              :status status)))

;;; Accept database credentials first, then fall back to PolarCL's default authorization.
(defun vh-authorizer (handler request)
  (or
   (authorize-user request)
   (default-authorizer handler request)))

;;; Authorize a registered function using database credentials or PolarCL's function authorizer.
(defun vh-function-authorizer (handler request function)
  (log2:trace "Authorizing ~a" function)
  (or
   (authorize-user request)
   (function-authorizer handler request function)))

;;; Log the function request and delegate authorization to PolarCL's default authorizer.
(defun vh-admin-authorizer (handler request function)
  (log2:trace "Authorizing ~a" function)
  (default-authorizer handler request))

;;; Check HTTP credentials against an active boat-name account and the MD5 hash of the supplied
;;; password.
(defun authorize-user (request)
  (sql:with-connection (*dbcon*)
    (multiple-value-bind (username password)
        (http-credentials request)
      (let ((user (get-user-by-boatname username)))
        (and user
             (string= (status user) "active")
             (string= (pwhash user) (md5 password)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unique identifiers

;;; Build a random 20-character identifier in three six-character groups; uniqueness is not
;;; checked.
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

;;; Return the mbedTLS SHA256 digest of MESSAGE in character form.
(defun sha256 (message)
  (mbedtls:mbedtls-md message :method "SHA256" :result-type :chars))

;;; Return the mbedTLS MD5 digest of MESSAGE in character form, as used by legacy account
;;; authentication.
(defun md5 (message)
  (mbedtls:mbedtls-md message :method "MD5" :result-type :chars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers - creating and resetting the database

;;; Create the Bitsailor schema in the configured SQLite file and return T; FORCE is currently
;;; unused.
(defun deploy (&key (force nil))
  (log2:info "Deploying to ~a" *db*)
  (sqlite-client:with-current-connection (c *db* :if-does-not-exist :create)
     (sql:create-schema (sql:get-schema-by-name "bitsailor")))
   (values t))

;;; Drop the Bitsailor schema through a connection to the configured SQLite file; FORCE is unused.
(defun drop (&key (force nil))
   (sqlite-client:with-current-connection (c *db* :if-does-not-exist :create)
     (sql:%drop-schema "bitsailor")))

;;; Clear schema data with SQLite foreign-key checks temporarily disabled on the connection.
(defun clear ()
  (sqlite-client:with-current-connection (c *db*)
    (sql:sql-exec c "PRAGMA foreign_keys = OFF;")
    (sql::clear-schema (sql:get-schema-by-name "bitsailor"))
    (sql:sql-exec c "PRAGMA foreign_keys = ON;")))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
