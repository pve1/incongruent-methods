(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system 'incongruent-methods)
  (use-package :incongruent-methods))

(define-incongruent-method simple ()
  'simple)

(define-incongruent-method hello ((me string))
  (concatenate 'string "Hello, " me))

(define-incongruent-method hello ((me string) (greeting string))
  (concatenate 'string greeting ", " me))

(define-incongruent-method hello ((me number) (number number))
  (+ me number))

(define-incongruent-method hello ((me number))
  "Hello, number!")

(defun incongruent-methods-test-1 ()
  (assert (eq (simple) 'simple))
  (assert (equal (hello "world") "Hello, world"))
  (assert (equal (hello "world" "Hey") "Hey, world"))
  (assert (equal (hello 1) "Hello, number!"))

  (flet ((f () #'hello))
    (assert (equal (funcall (f) "world") "Hello, world")))
  :ok)

(defun incongruent-methods-test-2 ()
  (incongruent-methods::remove-incongruent-function 'hello)

  (multiple-value-bind (val err)
      (ignore-errors (hello "Hello"))
    val
    (assert err))

  (multiple-value-bind (val err)
      (ignore-errors (hello 1 2))
    val
    (assert err))
  :ok)

(defun incongruent-methods-test-3 ()
  (assert (equal (incongruent-methods::dispatcher-compiler-macro
                  '(foo 1 2))
                 '(foo/2 1 2)))
  (assert (equal (incongruent-methods::dispatcher-compiler-macro
                  '(funcall 'foo 1 2))
                 '(funcall 'foo/2 1 2)))
  (assert (equal (incongruent-methods::dispatcher-compiler-macro
                  '(funcall #'foo 1 2))
                 '(funcall #'foo/2 1 2)))
  (assert (equal (incongruent-methods::dispatcher-compiler-macro
                  '(funcall (f) 1 2))
                 '(funcall (f) 1 2))))

(incongruent-methods-test-1)
(incongruent-methods-test-2)
(incongruent-methods-test-3)
