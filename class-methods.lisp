(in-package :incongruent-methods)

#+clisp
(defmethod find-method-imp (gf qualifiers specializers &optional errorp)
  (find-method gf
               qualifiers
               (mapcar
                (lambda (x)
                  (etypecase x
                    (list (clos::intern-eql-specializer (second x)))
                    (symbol (find-class x))
                    (class x)))
                specializers)
               errorp))

#+(or sbcl ccl)
(defmethod find-method-imp (gf qualifiers specializers &optional errorp)
  (find-method gf qualifiers specializers errorp))


(defvar *class-principal-methods* (make-hash-table :test 'eq))

(defun add-class-principal-method (name method-lambda-list)
  (let ((class (if (setf-method-p name)
                   (method-parameter-type
                    (second  method-lambda-list))
                   (method-parameter-type
                    (first method-lambda-list)))))
    (pushnew (list name (mapcar #'method-parameter-type
                                method-lambda-list))
             (gethash class *class-principal-methods*)
             :test 'equal)))

(defun remove-class-principal-methods (class-name)
  (let ((methods (gethash class-name *class-principal-methods*)))
    (loop :for (method-name lambda-list) :in methods
       :do (let* ((gf-with-arity
                    (ignore-errors
                     (find-method-with-arity
                      method-name
                      (method-lambda-list-arity lambda-list)))) ;; This works
                  (method
                    (when gf-with-arity
                      (find-method-imp
                       gf-with-arity
                       nil
                       lambda-list))))
             (when method
               (remove-method gf-with-arity method)
               (setf (gethash class-name *class-principal-methods*)
                     (delete (list method-name lambda-list)
                             (gethash class-name *class-principal-methods*)
                             :test #'equal)))))
    (unless (gethash class-name *class-principal-methods*)
      (remhash class-name *class-principal-methods*))))

(defmacro clear-class-methods (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (remove-class-principal-methods ',name)))

(defun list-class-principal-methods (class-name)
  (gethash class-name *class-principal-methods*))

(defmacro define-class-method (name method-lambda-list
                               &body body)
  `(progn
     (define-incongruent-method ,name ,method-lambda-list
       ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (add-class-principal-method ',name ',method-lambda-list))))

(defun without-props (plist props)
  (loop :for (options value) :on plist :by #'cddr
        :append (unless (member options props)
                  (list options value))))

(defmacro define-class (name direct-superclasses direct-slots
                        &body methods)
  (let ((me (gensym)))
    (labels ((slot-definition (x)
               (if (listp x)
                   (cons (first x)
                         (without-props (rest x)
                           '(:reader :writer :accessor)))
                   x))

             (slot-accessor-definition (x)
               (destructuring-bind (slot-name &rest options) x
                 (loop :for (options value) :on options :by #'cddr
                       :append
                       (case options
                         (:accessor
                          `((define-class-method ,value ((,me ,name))
                              (slot-value ,me ',slot-name))
                            (define-class-method (setf ,value)
                                (new (,me ,name))
                              (setf (slot-value ,me ',slot-name) new))))
                         (:reader
                          `((define-class-method ,value ((,me ,name))
                              (slot-value ,me ',slot-name))))
                         (:writer
                          `((define-class-method (setf ,value)
                                (new (,me ,name))
                              (setf (slot-value ,me ',slot-name) new))))))))

             (method-definition (definition)
               (destructuring-bind (method-name lambda-list &rest body)
                   definition
                 (if (setf-method-p method-name)
                     `(define-class-method ,method-name (,(first lambda-list)
                                                         (,(intern "ME") ,name)
                                                         ,@(rest lambda-list))
                        ,@body)
                     `(define-class-method ,method-name ((,(intern "ME") ,name)
                                                         ,@lambda-list)
                        ,@body)))))

      `(progn
         (defclass ,name ,direct-superclasses
           ,(mapcar #'slot-definition direct-slots))
         ,@(mapcan #'slot-accessor-definition
                   (remove-if-not #'listp direct-slots))
         ,@(mapcar #'method-definition methods)))))


;; Indentation for define-class.
;; requires slime-indentation

;; Alternatively, one could do
;; (put 'define-class
;;      'common-lisp-indent-function
;;      '(4 4 4 &rest (&whole 2 4 &lambda &body))
;; in emacs.

(defvar *indentation-hints* (make-hash-table :test 'eq))

(defun find-bound-symbol (symbol package)
  (let* ((%package (find-package package))
         (s (when %package
              (find-symbol (string symbol) %package))))
    (if (and s (boundp s) s)
        (values s (symbol-value s)))))

(defun update-indentation-hints ()
  (let ((tables-symbol (find-bound-symbol '#:*application-hints-tables*
                                          '#:swank)))
    (when tables-symbol
      (pushnew *indentation-hints* (symbol-value tables-symbol))
      (setf (gethash 'define-class *indentation-hints*)
            '(4 4 4 &rest (&whole 2 4 &lambda &body))))))

(update-indentation-hints)
