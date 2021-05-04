;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2021-05-02 21:29:06>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (log2:log-level "mbedtls") log2:+info+)
(setf (log2:log-level "mbedtls:accept") log2:+info+)
;; (setf (log2:log-level "mbedtls:mbedtls-net-accept") log2:+debug+)
(setf (log2:log-level "mbedtls:create-config") log2:+info+)
(setf (log2:log-level "mbedtls:create-ssl-env") log2:+info+)
(setf (log2:log-level "mbedtls:mbedtls-error-text") log2:+info+)
(setf (log2:log-level "mbedtls:mbedtls-net-accept") log2:+info+)
(setf (log2:log-level "mbedtls:mbedtls-ssl-read") log2:+info+) 
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

;;; Start one server on port 8080. 
(server :hostname "aguas-13" ;; Hostname binds to the WLAN/LAN interface! 
        :protocol :http
        :mt-method :ondemand
        ;; :mt-method :pooled
        :port "8080"
        :max-handlers 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----
;;; Users
;;; -----

(user :username "admin" :realm "virtualhelm" :password "_admin_01")
(user :username "admin" :realm "admin" :password "_admin_01")
(user :username "user01" :realm "virtualhelm" :password "_user_01")
(user :username "guest" :realm "virtualhelm" :password "_guest_01")

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
 :handler (:query-function t :realm "virtualhelm"))

(handle
 :request (:prefix "/public")
 :handler (:query-function t :authentication nil))

(register-function "vh.getSession")
(register-function "vh.getLegInfo")
(register-function "vh.getWind")
(register-function "vh.probeWind")
(register-function "vh.getWindForecast")
(register-function "vh.getTWAPath")
(register-function "vh.setParameter")
(register-function "vh.getRaceList" :authorizer (constantly t))
(register-function "vh.resetNMEAConnection")
(register-function "vh.getBoatPosition")
(register-function "vh.setRoute")
(register-function "vh.getRoute")
(register-function "vh.checkWindow")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------
;;; Static content
;;; ----------------

(handle
 :request (:prefix "/js")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web"))
                                     *source-root*))
                   :authentication nil))
(handle
 :request (:prefix "/css")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web"))
                                     *source-root*))
                   :authentication nil))

(handle
 :request (:prefix "/img")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web"))
                                     *source-root*))
                   :authentication nil))

(handle
 :request (:method :get
           :path "/start")
 :handler (:dynamic 'vh:get-page :realm "virtualhelm"))

(handle
 :request (:method :get
           :path "/intro")
 :handler (:dynamic 'vh:get-page
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
                      (declare (ignore server response))
                      (if (string= (http-authenticated-user handler request)
                                   "admin")
                          (progn (stop-all-servers)
                                 "<!DOCTYPE html><html><body><b><em>Goodby</em></b></body><html>")
                          "<!DOCTYPE html><html><body><b><em>Not authorized.</em></b></body><html>"))
                    :realm "admin"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
