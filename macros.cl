;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-09-13 00:47:56>

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Memoization

(defvar *memoized-functions-ht*
  (make-hash-table))

(defun memoize (f)
  (let* ((ff (symbol-function f))
         (entry (gethash f *memoized-functions-ht*)))
    (cond
      (entry
        (error "~a is already memoized, unmemoize it first" f))
      (t
        (setf (gethash f *memoized-functions-ht*) ff)
        (let ((cache (make-hash-table :test #'equal)))
          (setf (symbol-function f)
                (compile ()
                         (lambda (&rest args)
                           (or (gethash args cache)
                               (setf (gethash args cache)
                                     (apply ff args)))))))))))

(defun unmemoize (f)
  (let ((entry (gethash f *memoized-functions-ht*)))
    (cond
      (entry
        (setf (symbol-function f) entry)
        (setf (gethash f *memoized-functions-ht*) nil))
      (t
        (error "~a is not memoized" f)))))

(defmacro memoized (arg memo-ht form)
  `(multiple-value-bind (value found)
       (gethash ,arg ,memo-ht)
     (cond (found value)
           (T
            (setf (gethash ,arg ,memo-ht)
                  ,form)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
