(asdf:defsystem #:incongruent-methods
  :serial t
  :depends-on (:closer-mop)
  :components ((:file "package")
               (:file "incongruent-methods")
               (:file "shared-methods")
               (:file "class-methods")))
