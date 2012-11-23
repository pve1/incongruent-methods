(in-package :cl-user)

(defpackage :incongruent-methods
  (:use :cl)
  (:export #:define-incongruent-method
           #:remove-incongruent-function
           #:define-class
           #:define-class-method))
