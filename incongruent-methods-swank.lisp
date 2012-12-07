;; Load this from ~/.swank.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :incongruent-methods))

(in-package :swank-backend)

(defvar *find-definitions-functions* nil)

#+sbcl
(defimplementation find-definitions (name)
  (loop for type in *definition-types* by #'cddr
        for defsrcs = (apply #'append
                             (sb-introspect:find-definition-sources-by-name name type)
                             (mapcar (lambda (f) (funcall f name type))
                                     *find-definitions-functions*))
        append (loop for defsrc in defsrcs collect
                     (list (make-dspec type name defsrc)
                           (converting-errors-to-error-location
                             (definition-source-for-emacs defsrc
                                 type name))))))


;;;;

(in-package :incongruent-methods)

#+sbcl
(defun find-incongruent-method-definition-source-by-name (name type)
  (when (eq type :method)
    (mapcar #'sb-introspect:find-definition-source
            (list-incongruent-methods name))))

(pushnew 'find-incongruent-method-definition-source-by-name
         swank-backend::*find-definitions-functions*)

(update-indentation-hints)
