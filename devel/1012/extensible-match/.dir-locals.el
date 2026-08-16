;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((scheme-mode . (;; Scheme pattern matching macros
                 (eval . (put 'match 'scheme-indent-function 1))
                 (eval . (put 'match-values 'scheme-indent-function 1))
                 (eval . (put 'match-let 'scheme-indent-function 1))
                 (eval . (put 'match-let* 'scheme-indent-function 1))
                 (eval . (put 'match-let*-values 'scheme-indent-function 1))
                 (eval . (put 'match-let-values 'scheme-indent-function 1))
                 (eval . (put 'match-letrec 'scheme-indent-function 1))
                 (eval . (put 'match-letrec* 'scheme-indent-function 1))
                 (eval . (put 'match-define 'scheme-indent-function 'defun))
                 (eval . (put 'match-define-values 'scheme-indent-function 'defun))
                 (eval . (put 'match-lambda 'scheme-indent-function 0))
                 (eval . (put 'if-match 'scheme-indent-function 1))

                 ;; Built-in pattern syntax
                 (eval . (put 'seq 'scheme-indent-function 4))
                 (eval . (put 'seq* 'scheme-indent-function 4))
                 (eval . (put 'seq/unordered 'scheme-indent-function 4))

                 ;; Pattern syntax definitions
                 (eval . (put 'define-pattern-syntax 'scheme-indent-function 'defun))

                 ;; Internal macros
                 (eval . (put 'core-pattern-case 'scheme-indent-function 2))
                 (eval . (put 'ast-cond 'scheme-indent-function 1))
                 )))
