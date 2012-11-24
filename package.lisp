;; This software is Copyright (c) Peter von Etter, 2012.
;; You can distribute and use it as governed by the terms of the Lisp
;; Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

(in-package :cl-user)

(defpackage :incongruent-methods
  (:use :cl)
  (:export #:define-incongruent-method
           #:remove-incongruent-function
           #:define-class
           #:define-class-method))
