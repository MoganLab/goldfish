;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck arity internal scope)
  (import (scheme base))
  (export %resolve-binding %scope-extend %scope-shadow)
  (begin
    (define (%resolve-binding name env)
      (let ((binding (and (symbol? name) (assq name env))))
        (cond (binding (cdr binding))
              ((and (symbol? name) (memq name env)) name)
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%scope-extend bindings env)
      (append
        (map
          (lambda (binding)
            (if (eq? (%resolve-binding (car binding) env) 'mutated)
              (cons (car binding) 'mutated)
              binding
            ) ;if
          ) ;lambda
          bindings
        ) ;map
        env
      ) ;append
    ) ;define

    (define (%scope-shadow names env)
      (%scope-extend (map (lambda (name) (cons name 'shadow)) names) env)
    ) ;define
  ) ;begin
) ;define-library
