(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :incongruent-methods)
  (use-package :incongruent-methods))

(define-incongruent-method hello ((me string))
  (concatenate 'string "Hello, " me))

(define-incongruent-method hello ((me string) (greeting string))
  (concatenate 'string greeting ", " me))

(define-incongruent-method hello ((me number) (number number))
  (+ me number))

(define-incongruent-method hello ((me number))
  "Hello, number!")

(defun incongruent-methods-test-1 ()
  (assert (equal (hello "world") "Hello, world"))
  (assert (equal (hello "world" "Hey") "Hey, world"))
  (assert (equal (hello 1) "Hello, number!"))
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

(incongruent-methods-test-1)
(incongruent-methods-test-2)

