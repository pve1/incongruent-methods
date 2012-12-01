
(put 'define-class
     'common-lisp-indent-function
     '(4 4 2 &rest (&whole 2 4 &lambda &body)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(define-incongruent-method \\(\\(\\sw\\|\\s_\\)+\\)"
                1 font-lock-function-name-face prepend)
               ("(define-shared-method +\\(\\(\\sw\\|\\s_\\)+\\)"
                1 font-lock-function-name-face prepend)
               ("(define-class-method +\\(\\(\\sw\\|\\s_\\)+\\)"
                1 font-lock-function-name-face prepend)
               ("(define-class +\\(\\(\\sw\\|\\s_\\)+\\)"
                1 font-lock-type-face prepend)))))

(provide 'incongruent-methods)
