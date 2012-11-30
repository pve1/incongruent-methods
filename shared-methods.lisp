(in-package :incongruent-methods)

(defpackage :incongruent-methods.methods)

(defvar *method-package* (find-package :incongruent-methods.methods))

(defun clear-shared-methods ()
  (do-symbols (s *method-package*)
    (unintern s *method-package*)))

(defgeneric intern-shared-method (thing))
(defgeneric find-shared-method (name))
(defgeneric imcall (method &rest args))
(defgeneric send (object method &rest args))

(defmethod intern-shared-method ((name symbol))
  (let ((name (intern (string name) *method-package*)))
    name))

(defmethod intern-shared-method ((name list))
  (let ((name (list (first name)
                    (intern (string (second name))
                            *method-package*))))
    name))

(defmethod find-shared-method ((name symbol))
  (find-shared-method (string name)))

(defmethod find-shared-method ((name string))
  (find-symbol name *method-package*))

(defmethod find-shared-method ((name list))
  (let ((n (find-shared-method (second name))))
    (when n
      (list (first name) n))))

(define-modify-macro internf-shared-method () intern-shared-method)

(defmacro define-shared-method (name method-lambda-list &body body)
  (let ((interned-name (intern-shared-method name)))
    `(define-incongruent-method ,interned-name ,method-lambda-list
       ,@body)))

(defun list-shared-methods (name)
  (let ((name-symbol (find-shared-method name)))
    (when name-symbol
      (list-incongruent-methods name-symbol))))

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
           (cond ((and (listp method)
                       (eq (car method) 'quote))
                  (destructuring-bind (quote symbol) method
                    (ecase quote
                      (quote
                       (dispatcher-compiler-macro
                        `(,(intern-shared-method symbol) ,@args))))))

                 ((keywordp method)
                  (dispatcher-compiler-macro
                   `(,(intern-shared-method method) ,@args)))

                 (t whole))))
    #+debug-incongruent-methods
    (format t "IMCALL compiler-macro whole: ~S~%" whole)
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
           (cond ((and (listp method)
                       (eq (car method) 'quote))
                  (destructuring-bind (quote symbol) method
                    (ecase quote
                      (quote `(setf ,(butlast ;; Hack
                                      (dispatcher-compiler-macro
                                       `(,(intern-shared-method symbol)
                                         ,@(append args (list ''dummy)))))
                                    ,new)))))

                 ((keywordp method)
                  `(setf ,(butlast ;; Hack
                           (dispatcher-compiler-macro
                            `(,(intern-shared-method method)
                              ,@(append args (list ''dummy)))))
                         ,new))

                 (t whole))))

    #+debug-incongruent-methods
    (format t "~&(SETF IMCALL) compiler-macro whole: ~S~%" whole)
    #+debug-incongruent-methods
    (let ((exp (make-form)))
      (format t "(SETF IMCALL) compiler-macro expansion: ~S~%" exp)
      exp)
    #-debug-incongruent-methods
    (make-form)))
