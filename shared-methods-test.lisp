(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :incongruent-methods)
  (use-package :incongruent-methods))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (incongruent-methods::clear-shared-methods))

(define-shared-method hello (x)
  (concatenate 'string "Hello, " x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((var 1))
    (define-shared-method a-var ()
      var)
    (define-shared-method (setf a-var) (new)
      (setf var new))))

(defpackage :incongruent-methods.test-package-1
  (:use :cl :incongruent-methods))

(in-package :incongruent-methods.test-package-1)

(defclass myclass ()
  ((slot :accessor %get-slot)))

(define-shared-method slot ((me myclass))
  (slot-value me 'slot))

(define-shared-method get-something ((me myclass))
  'something)


(defpackage :incongruent-methods.test-package-2
  (:use :cl :incongruent-methods))

(in-package :incongruent-methods.test-package-2)

(defclass myclass-2 () ())

(define-shared-method a-method ((me myclass-2)
                                an-object)
  (imcall 'get-something an-object))

(define-shared-method test ((me (eql :incongruent-methods.test-package-2)))
  (let ((myobject2 (make-instance 'myclass-2))
        (myobject (make-instance 'incongruent-methods.test-package-1::myclass)))
    (imcall 'a-method myobject2 myobject)))

(in-package :cl-user)

(defun shared-methods-test-1 ()
  (assert (= (length (list-shared-methods 'hello)) 1))

  (assert (string= (imcall 'hello "world")
                   "Hello, world"))
  (setf (imcall 'a-var) 1)
  (assert (= 1 (imcall 'a-var)))
  (setf (imcall 'a-var) 2)
  (assert (= 2 (imcall 'a-var)))

  (setf (imcall :a-var) 1)
  (assert (= 1 (imcall :a-var)))
  (setf (imcall :a-var) 2)
  (assert (= 2 (imcall :a-var)))

  (flet ((f () 'hello)
         (g () :hello))
    (assert (string= (imcall (f) "world") "Hello, world"))
    (assert (string= (imcall (g) "world") "Hello, world")))
  :ok)

(defun shared-methods-test-2 ()
  (assert (eq (imcall 'test :incongruent-methods.test-package-2)
              'incongruent-methods.test-package-1::something))
  :ok)

(defun shared-methods-test-3.1 ()
  (imcall 'echo 'hello)) ;; Should generate style warning.

(define-shared-method echo ((x symbol))
  x)

(defun shared-methods-test-3 ()
  (assert (eq 'hello (shared-methods-test-3.1))))



(shared-methods-test-1)
(shared-methods-test-2)
(shared-methods-test-3)
