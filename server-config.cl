;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2022-01-08 17:14:16>

(in-package :virtualhelm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (log2:log-level "virtualhelm") log2:+info+)
(setf (log2:log-level "cl-weather") log2:+info+)
(setf (log2:log-level "cl-map") log2:+info+)
(setf (log2:log-level "polarcl") log2:+info+)
(setf (log2:log-level "mbedtls") log2:+info+)

(setf (log2:log-level "mbedtls:refresh-buffer") log2:+warning+)
(setf (log2:log-level "polarcl:server-loop-ondemand") log2:+trace+)
(setf (log2:log-level "polarcl:handler-thread") log2:+trace+)
(setf (log2:log-level "virtualhelm:log-stats") log2:+trace+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------
;;; Servers
;;; -------

(format t "~a"
        (merge-pathnames (make-pathname :directory '(:relative "web"))
                         (make-pathname :directory (pathname-directory *load-pathname*))))

;;; Start one server on port 8080. 
(server :hostname "localhost" ;; "aguas-13" ;; Hostname binds to the WLAN/LAN interface! 
        :protocol :http
        :mt-method :ondemand
        ;; :mt-method :pooled
        :port "8080"
        :max-handlers 3)
          
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
 :from (:path "/")
 :to (:path "/start"))

(redirect
 :from (:regex ".*/^")
 :to (:path "index.html"))

(redirect
 :from (:scheme "http" :port ("8088"))
 :to (:scheme "https" :port "443"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------------
;;; QUERY-FUNCTION endpoint
;;; -------------

(handle
 :request (:prefix "/function")
 :handler (:query-function t :authentication nil :realm "virtualhelm"))

(register-function "vh.signUp" :authorizer (constantly t))
(register-function "vh.getSession" :authorizer #'vh-function-authorizer)
(register-function "vh.removeSession" :authorizer #'vh-function-authorizer)
(register-function "vh.getRaceInfo" :authorizer #'vh-function-authorizer)
(register-function "vh.getWind" :authorizer #'vh-function-authorizer)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polars -

(handle
 :request ( :method :get
            :path "/polars")
 :handler ( :directory  "/home/michael/Polars"
            :authentication nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRIBs

(handle
 :request (:method :get
           :path "/weather")
 :handler (:directory "/home/michael/Wetter"
           :authentication nil))

(handle
 :request (:prefix "/weather")
 :handler (:static "/home/michael/Wetter/"
           :authentication nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web page

(handle
 :request (:prefix "/js")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web" "js"))
                                     *source-root*))
           :authentication nil))
(handle
 :request (:prefix "/css")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web" "css"))
                                     *source-root*))
           :authentication nil))

(handle
 :request (:prefix "/img")
 :handler (:static (namestring
                    (merge-pathnames (make-pathname :directory '(:relative "web" "img"))
                                     *source-root*))
           :authentication nil))

(handle
 :request (:prefix "/polars")
 :handler (:static (namestring *polars-dir*)
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
