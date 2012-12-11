(asdf:defsystem #:incongruent-methods
  :version "0.1"
  :author "Peter von Etter"
  :description "Methods with incongruent lambda lists."
  :license "LLGPL"
  :serial t
  :depends-on (:closer-mop)
  :components ((:file "package")
               (:file "incongruent-methods")
               (:file "shared-methods")
               (:file "class-methods")))
