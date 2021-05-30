;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2021-05-30 17:19:29>

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

(server :hostname "aguas-13" ;; Hostname binds to the WLAN/LAN interface! 
        :protocol :http
        :mt-method :ondemand
        ;; :mt-method :pooled
        :port "8088"
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

(redirect
 :from (:scheme "http" :port ("8088"))
 :to (:scheme "https" :port "443"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------------
;;; QUERY-FUNCTION endpoint
;;; -------------

;;; TODO: Function handlers don't require separate authorization.
;;;       Remove one endpoint.

(handle
 :request (:prefix "/function")
 :handler (:query-function t :realm "virtualhelm" :authorizer #'vh-authorizer))

(handle
 :request (:prefix "/public")
 :handler (:query-function t :authentication nil))

(register-function "vh.signUp" :authorizer (constantly t))
(register-function "vh.getSession" :authorizer #'vh-function-authorizer)
(register-function "vh.getLegInfo" :authorizer #'vh-function-authorizer)
(register-function "vh.getWind" :authorizer #'vh-function-authorizer)
(register-function "vh.probeWind" :authorizer #'vh-function-authorizer)
(register-function "vh.getWindForecast" :authorizer #'vh-function-authorizer)
(register-function "vh.getTWAPath" :authorizer #'vh-function-authorizer)
(register-function "vh.setParameter" :authorizer #'vh-function-authorizer)
(register-function "vh.getRaceList" :authorizer (constantly t))
(register-function "vh.resetNMEAConnection" :authorizer #'vh-function-authorizer)
(register-function "vh.getBoatPosition" :authorizer #'vh-function-authorizer)
(register-function "vh.setRoute" :authorizer #'vh-function-authorizer)
(register-function "vh.getRoute" :authorizer #'vh-function-authorizer)
(register-function "vh.getRouteRS" :authorizer #'vh-function-authorizer)
(register-function "vh.checkWindow" :authorizer #'vh-function-authorizer)

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
 :request (:prefix "/polars")
 :handler (:static (namestring  *source-root*)
           :authentication nil))

(handle
 :request (:method :get
           :path "/start")
 :handler (:dynamic 'start-page
           :authentication nil))

(handle
 :request (:method :get
           :path "/router")
 :handler (:dynamic 'router
           :realm "virtualhelm"
           :authorizer #'vh-authorizer))
(handle 
 :request (:method :get
           :prefix "/activate-account")
 :handler (:dynamic 'activate-account
           :authentication nil))
;;; We can't match root for now, match length priority is not implemented or does not work...
(handle 
 :request (:prefix "")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web"))
                                     *source-root*))
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
