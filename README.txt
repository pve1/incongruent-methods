* Incongruent methods

** Basics

Incongruent methods is simple implementation of
incongruent (overloaded) methods using standard CLOS methods.  These
methods do not support &key, &rest, &optional etc.

Example:

  (define-incongruent-method hello ((me string))
    (concatenate 'string "Hello, " me))

  (define-incongruent-method hello ((me string) (greeting string))
    (concatenate 'string greeting ", " me))

  (hello "world")
  ==> "Hello world"

  (hello "world" "Hey")
  ==> "Hey world"


Example 2:

  (defclass hello ()
    ((message :initarg :message
              :initform nil
              :accessor %get-message))) ;; standard generic accessor.

  (define-incongruent-method greet ((me hello))
    (print (%get-message me)))

  (define-incongruent-method greet ((me hello) (n integer))
    (dotimes (m n)
      (greet me)))


** Define-class

A convenience macro for defining classes is also provided.  The
symbol "ME" in the body of the methods means the current object (self,
this etc).  It should also be the first argument when calling methods
defined using this macro.  Accessors will be converted to incongruent
methods.

Example:

(define-class greeting ()
    ((message :accessor message :initform nil)
     (some-slot :accessor some-slot :initform nil))

  ;; Methods can be defined here, if one wants.

  (greet () ;; lambda list is actually ((me greeting))
    (message me))

  (greet ((n integer)) ;; lambda list is actually ((me greeting) (n integer))
    (dotimes (m n)
      (greet me)))

  (greet ((n (eql 42)))
    (some-slot me))

  ((setf greet) (new)
    (setf (message me) new)))


(let ((i (make-instance 'greeting :message "Hello")))
  (greet i))

==> "Hello"

(let ((i (make-instance 'greeting :message "Hello")))
  (setf (greet i) "Hi")
  (greet i))

==> "Hi"


** Shared methods

Methods that are "shared" between packages can be also be defined.
These methods can be called using the "imcall" function.  It is
advisable that shared methods are specialized on at least one class
defined in your own system.  A shared method specializing only on,
say, the built-in integer class, might accidentally get redefined by
other systems.

- Operations that are part of the "public interface" of a class are
  good candidates for shared methods.

- In the following example, notice how the call to "notify" in the
  do-something method refers to test-package-1::notify, not
  test-package-2::notify.  In fact, test-package-1 has no knowledge of
  the other package and can be compiled separately if one wishes.


Example:

  (defpackage :test-package-1
    (:use :cl :incongruent-methods)
    (:export #:my-class))

  (in-package :test-package-1)

  (defclass my-class ()
    ((slot :accessor %get-slot)))

  (define-shared-method slot-contents ((me my-class))
    (slot-value me 'slot))

  (define-shared-method get-something ((me my-class))
    'something)

  (define-shared-method do-something ((me my-class)
                                      (object t))
    (format t "I did something!~%")
    (imcall 'notify object :done))

  ;;;;

  (defpackage :test-package-2
    (:use :cl :incongruent-methods))

  (in-package :test-package-2)

  (defclass some-random-class () ())

  (define-shared-method notify ((me some-random-class)
                                (status symbol))
    (format t "Received status: ~A~%" status))

  (defun main ()
    (let ((my-object (make-instance 'test-package-1:my-class))
          (some-object (make-instance 'some-random-class)))
      (format t "~S~%" (imcall 'get-something my-object))
      (imcall 'do-something my-object some-object)))


  CL-USER> (main)

  ==>

  TEST-PACKAGE-1::SOMETHING
  I did something!
  Received status: DONE

