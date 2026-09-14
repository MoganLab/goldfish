(define-library (liii tree)
  (export tree-cyclic? tree-leaves tree-memq? tree-member? tree-set-memq
    tree-count tree-depth
  ) ;export
  (import (scheme base) (liii error))
  (begin
    (define (quote-form? x)
      (and (pair? x)
        (or (eq? (car x) 'quote) (eq? (car x) #_quote))
        (pair? (cdr x))
        (null? (cddr x))
      ) ;and
    ) ;define

    (define (tree-depth tree)
      (if (tree-cyclic? tree)
        (value-error "tree-depth: tree is cyclic: ~S" tree)
        (let loop
          ((x tree))
          (cond ((not (pair? x)) 0)
                ((quote-form? x) (loop (cadr x)))
                (else (+ 1
                        (let elt-loop
                          ((rest x) (max-d 0))
                          (if (not (pair? rest))
                            max-d
                            (elt-loop (cdr rest) (max max-d (loop (car rest))))
                          ) ;if
                        ) ;let
                      ) ;+
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
