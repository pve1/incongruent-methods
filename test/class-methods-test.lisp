(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :incongruent-methods)
  (use-package :incongruent-methods))

(defclass hello ()
  ((message :initarg :message
            :initform nil
            :accessor %get-message)))

(define-class-method get-message ((me hello))
  (%get-message me))

(define-class-method get-message ((me hello)
                                  (n number))
  (let ((result))
    (dotimes (s n)
      (push (get-message me) result))
    result))

(define-class-method say ((me hello))
  (get-message me))

(defun class-method-test-1 ()
  (let ((hello (make-instance 'hello :message "Hello")))
    (assert (equal "Hello" (get-message hello)))
    (assert (equal '("Hello" "Hello") (get-message hello 2)))
    (assert (equal "Hello" (say hello)))
    :ok))

(define-class greeting (hello)
    ((x :accessor x :initform nil)
     (y :accessor y :initform 2)
     (z :initform 3))

  (greet ()
    (slot-value me 'message))

  (greet ((n integer))
    (get-message me n))

  (greet ((n (eql 42)))
    (get-message me n))

  ((setf greet) (new)
    (setf (slot-value me 'message) new)))

(defun class-method-test-2 ()
  (let ((greeting (make-instance 'greeting :message "Greetings")))

    (assert (eq nil (x greeting)))
    (assert (= 2 (y greeting)))
    (assert (= 3 (slot-value greeting 'z)))

    (setf (x greeting) 10)
    (assert (= 10 (x greeting)))

    (assert (equal "Greetings" (greet greeting)))
    (assert (equal '("Greetings" "Greetings") (greet greeting 2)))

    (assert (equal '("Greetings" "Greetings") (get-message greeting 2)))
    (assert (equal "Greetings" (say greeting)))

    (setf (greet greeting) "Hey")
    (assert (equal '("Hey" "Hey") (greet greeting 2)))
    :ok))

(defun class-method-test-3 ()
  (let ((greeting (make-instance 'greeting :message "Greetings")))
    (assert (= 8 (length
                  (incongruent-methods::list-class-principal-methods
                   'greeting))))

    (incongruent-methods::remove-class-principal-methods
     'greeting)

    (multiple-value-bind (val err1)
        (ignore-errors (greet greeting))
      val
      (assert err1))

    (multiple-value-bind (val err2)
        (ignore-errors (setf (greet greeting) "Hello"))
      val
      (assert err2))
    :ok))

(class-method-test-1)
(class-method-test-2)
(class-method-test-3)
