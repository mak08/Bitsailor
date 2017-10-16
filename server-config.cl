;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2017-10-16 22:38:51>

(setf (log2:log-level "mbedtls") log2:+info+)
(setf (log2:log-level "mbedtls:accept") log2:+info+)
;; (setf (log2:log-level "mbedtls:mbedtls-net-accept") log2:+debug+)
(setf (log2:log-level "mbedtls:create-config") log2:+info+)
(setf (log2:log-level "mbedtls:create-ssl-env") log2:+info+)
(setf (log2:log-level "mbedtls:mbedtls-error-text") log2:+info+)

(setf (log2:log-level "polarcl") log2:+info+)
(setf (log2:log-level "polarcl:server-loop-ondemand") log2:+info+)
(setf (log2:log-level "polarcl:handler-thread") log2:+info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------
;;; Servers
;;; -------

(format t "~a"
        (merge-pathnames (make-pathname :directory '(:relative "web"))
                         (make-pathname :directory (pathname-directory *load-pathname*))))

;;; Start one server on port 8080 
(server :hostname "aguas-10"
        :protocol :http
        :mt-method :ondemand
        :port "8080"
        :max-handlers 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----
;;; Users
;;; -----

(user :username "admin" :realm "admin" :password "admin")
(user :username "guest" :realm "localhost" :password "guest")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----------
;;; Redirection : Every request referring to a directory is mapped to index.html in that directory
;;; -----------

(redirect
 :from (:regex ".*/")
 :to (:path "index.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------------
;;; QUERY-FUNCTION endpoint
;;; -------------

(handle
 :request (:prefix "/function")
 :handler (:query-function t :authentication nil))

(register-function 'vh:|getSession|)
(register-function 'vh:|getWind|)
(register-function 'vh:|setRoute|)
(register-function 'vh:|setParameter|)
(register-function 'vh:|getRoute|)
(register-function 'vh:|getTWAPath|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------
;;; Static content
;;; ----------------

(handle
 :request (:prefix "/vh")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web"))
                                     (make-pathname :directory (pathname-directory *load-pathname*))))
                   :authentication nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------
;;; Administration
;;; ----------------


;;; A :dynamic handler calls the specified function on the matched request and
;;; and a default "OK" response. Login as 'admin' required.
(handle
 :request (:method :get
           :path "/quit")
 :handler (:dynamic (lambda (server handler request response)
                                       (declare (ignore response))
                                       (if (string= (http-authenticated-user handler request)
                                                    "admin")
                                           (progn (stop-all-servers)
                                                  "<!DOCTYPE html><html><body><b><em>Goodby</em></b></body><html>")
                                           "<!DOCTYPE html><html><body><b><em>Not authorized.</em></b></body><html>"))
                    :realm "admin"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
