(in-package :incongruent-methods)

(defpackage :incongruent-methods.methods)

(defvar *method-package* (find-package :incongruent-methods.methods))

(defun clear-shared-methods ()
  (do-symbols (s *method-package*)
    #-clisp (fmakunbound s)
    (unintern s *method-package*)))

(defgeneric intern-shared-method (thing))
(defgeneric find-shared-method (name))
(defgeneric imcall (method &rest args))
(defgeneric send (object method &rest args))

(defmethod intern-shared-method ((name symbol))
  (intern (string name) *method-package*))

(defmethod intern-shared-method ((name list))
  (list (first name) (intern-shared-method (second name))))

(defmethod find-shared-method ((name symbol))
  (find-shared-method (string name)))

(defmethod find-shared-method ((name string))
  (find-symbol name *method-package*))

(define-modify-macro internf-shared-method () intern-shared-method)

(defmacro define-shared-method (name method-lambda-list &body body)
  (let ((interned-name (intern-shared-method name)))
    `(define-incongruent-method ,interned-name ,method-lambda-list
       ,@body)))

(defgeneric imcall (method &rest args))

(defmethod imcall ((method symbol) &rest args)
  (let* ((cache (load-time-value (make-hash-table :test 'eq)))
         (sym (or (gethash method cache)
                  (let ((s (find-shared-method method)))
                    (when s
                      (setf (gethash method cache) s))))))
    (apply (fdefinition sym) args)))

(define-compiler-macro imcall (&whole whole method &rest args)
  (flet ((make-form ()
           (cond ((listp method)
                  (destructuring-bind (quote-or-fun symbol) method
                    (ecase quote-or-fun
                      (quote `(,(find-shared-method symbol) ,@args)))))
                 (t whole))))
    #+debug-incongruent-methods
    (format t "~&IMCALL compiler-macro whole: ~S~%" whole)
    #+debug-incongruent-methods
    (let ((exp (make-form)))
      (format t "IMCALL compiler-macro expansion: ~S~%" exp)
      exp)
    #-debug-incongruent-methods
    (make-form)))


(defun (setf imcall) (new method &rest args)
  (let ((m (find-setf-method-with-arity
            (find-shared-method method)
            (1+ (method-lambda-list-arity args)))))
    (apply m new args)))

(define-compiler-macro (setf imcall) (&whole whole new method &rest args)
  (flet ((make-form ()
           (cond ((listp method)
                  (destructuring-bind (quote symbol) method
                    (ecase quote
                      (quote `(setf (,(find-shared-method symbol) ,@args)
                                    ,new)))))
                 (t whole))))

    #+debug-incongruent-methods
    (format t "~&(SETF IMCALL) compiler-macro whole: ~S~%" whole)
    #+debug-incongruent-methods
    (let ((exp (make-form)))
      (format t "(SETF IMCALL) compiler-macro expansion: ~S~%" exp)
      exp)
    #-debug-incongruent-methods
    (make-form)))
