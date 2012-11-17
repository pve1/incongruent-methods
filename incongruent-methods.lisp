(in-package :incongruent-methods)

(defun method-name-with-arity (name arity)
  (intern (format nil "~A-~A" name arity) (symbol-package name)))

(defun method-parameter-name (x)
  (etypecase x
    (list (first x))
    (symbol x)))

(defun method-lambda-list-arity (lambda-list)
  (length lambda-list))

(defgeneric find-method-with-arity (name arity))

(defun ensure-dispatcher (name)
  (setf (symbol-function name)
        (lambda (&rest rest)
          (apply (find-method-with-arity name
                                         (method-lambda-list-arity
                                          rest))
                 rest)))
  (setf (compiler-macro-function name)
        (lambda (form env)
          (declare (ignore env))
          (if (eq (car form) 'funcall)
              (destructuring-bind (funcall (fun-or-quote name) &rest args) form
                `(,funcall (,fun-or-quote ,(method-name-with-arity
                                            name
                                            (method-lambda-list-arity args)))
                           ,@args))
              (destructuring-bind (fun &rest args) form
                `(,(method-name-with-arity fun (method-lambda-list-arity args))
                   ,@args))))))

(defvar *generic-arity-functions* (make-hash-table :test 'eq))

(defun incongruent-function-p (name)
  (and (fboundp name)
       (gethash name *generic-arity-functions*)))

(defun ensure-generic-arity-function (name arity)
  (ensure-dispatcher name)
  (ensure-generic-function (method-name-with-arity name arity))
  (pushnew arity (gethash name *generic-arity-functions*)))

(defun remove-incongruent-function (name)
  (when (incongruent-function-p name)
    (fmakunbound name)
    (dolist (arity (gethash name *generic-arity-functions*))
      (fmakunbound (method-name-with-arity name arity)))
    (remhash name *generic-arity-functions*)))

(defmacro define-incongruent-method (name method-lambda-list
                                     &body body)
  (let* ((arity (method-lambda-list-arity method-lambda-list))
         (method-name (method-name-with-arity name arity)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (ensure-generic-arity-function ',name ,arity))
       (defmethod find-method-with-arity ((me (eql ',name))
                                          (arity (eql ,arity)))
         (function ,(method-name-with-arity name arity)))
       (defmethod ,method-name ,method-lambda-list
         ,@body))))

(pushnew :incongruent-methods *features*)
