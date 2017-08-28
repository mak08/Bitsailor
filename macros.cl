;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-08-24 20:08:07>

(in-package :virtualhelm)

(defmacro with-bindings (bindings &body forms)
  (let* ((bindings (reverse bindings))
         (head (car bindings))
         (rest (cdr bindings)))
    (loop
       :with form = `(multiple-value-bind ,(car head) ,(cadr head) ,@forms)
       :for binding :in rest
       :do (setf form `(multiple-value-bind ,(car binding) ,(cadr binding) ,form))
       :finally (return form))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
