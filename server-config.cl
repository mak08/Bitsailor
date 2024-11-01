;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2024-11-01 18:47:07>

(in-package :bitsailor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (log2:log-level "bitsailor") log2:+info+)
(setf (log2:log-level "cl-weather") log2:+info+)
(setf (log2:log-level "cl-map") log2:+info+)
(setf (log2:log-level "polarcl") log2:+info+)
(setf (log2:log-level "mbedtls") log2:+info+)

(setf (log2:log-level "mbedtls:refresh-buffer") log2:+warning+)
(setf (log2:log-level "polarcl:server-loop-ondemand") log2:+trace+)
(setf (log2:log-level "polarcl:handler-thread") log2:+trace+)
(setf (log2:log-level "bitsailor:log-stats") log2:+trace+)

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
        :max-handlers 10)
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -----
;;; Users
;;; -----

(user :username "admin" :realm "bitsailor" :password "_admin_01")
(user :username "admin" :realm "admin" :password "_admin_01")
(user :username "user01" :realm "bitsailor" :password "_user_01")
(user :username "guest" :realm "bitsailor" :password "_guest_01")

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
 :handler (:query-function t :authentication nil :realm "bitsailor"))

(handle
 :request (:prefix "/admin")
 :handler (:query-function t :authentication nil :realm "admin"))

(register-function "router.signUp" :authorizer (constantly t))
(register-function "router.getSession" :authorizer #'vh-function-authorizer)
(register-function "router.removeSession" :authorizer #'vh-function-authorizer)
(register-function "router.getRaceInfo" :authorizer #'vh-function-authorizer)
(register-function "router.getWind" :authorizer #'vh-function-authorizer)
(register-function "router.getTWAPath" :authorizer #'vh-function-authorizer)
(register-function "router.setParameter" :authorizer #'vh-function-authorizer)
(register-function "router.getRaceList" :authorizer (constantly t))
(register-function "router.getPolarsList" :authorizer  #'vh-admin-authorizer)
(register-function "router.getRaceListAdmin" :authorizer #'vh-admin-authorizer)
(register-function "router.setRoute" :authorizer #'vh-function-authorizer)
(register-function "router.getRoute" :authorizer #'vh-function-authorizer)
(register-function "router.getStatistics" :authorizer (constantly t))
(register-function "router.checkWindow" :authorizer #'vh-function-authorizer)

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
           :prefix "/weather")
 :handler (:directory "/home/michael/Wetter"
           :realm "bitsailor"
           :authorizer #'vh-authorizer))
#|
(handle
 :request (:method :get
              :path "/archive")
 :handler (:directory "/home/michael/Wetter/archive"
           :authentication nil))

(handle
 :request (:prefix "/archive")
 :handler (:static "/home/michael/Wetter/archive/"
           :realm "bitsailor"
           :authorizer #'vh-authorizer))
|#

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
           :realm "bitsailor"
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

(handle
 :request (:method :post
           :path "/checkPaths")
 :handler (:dynamic 'check-paths
           :authentication nil))

(handle
 :request (:method :options
           :prefix "")
 :handler (:dynamic 'handle-options
           :authentication nil))

(handle
 :request (:method :post
           :prefix "/executeCommands")
 :handler (:dynamic 'execute-commands
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
 :handler (:dynamic  (lambda (server handler request response)
                       (declare (ignore server response))
                       (cond
                         ((string= (http-authenticated-user handler request)
                                   "admin")
                          (stop-all-servers)
                          (setf *run* nil)
                          (values
                           "<!DOCTYPE html><html><body><b><em>Goodby</em></b></body><html>"))
                         (t
                          (values
                           "<!DOCTYPE html><html><body><b><em>Not authorized.</em></b></body><html>"))))
                    :realm "admin"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
