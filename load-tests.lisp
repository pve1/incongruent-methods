(require :asdf)
(push :debug-incongruent-methods *features*)
(load "incongruent-methods.asd")
(asdf:load-system :incongruent-methods)
(load (compile-file "test/incongruent-methods-test.lisp"))
(load (compile-file "test/shared-methods-test.lisp"))
(load (compile-file "test/class-methods-test.lisp"))
(princ "OK")
(terpri)
(quit)
