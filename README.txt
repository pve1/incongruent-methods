A simple implementation of incongruent (overloaded) methods using
standard CLOS methods.  These methods do not support &key, &rest,
&optional etc.

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
    (message me n))

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
