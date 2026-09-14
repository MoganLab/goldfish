(define-library (liii tree)
  (export tree-cyclic? tree-leaves tree-memq? tree-member? tree-set-memq
    tree-count tree-depth
  ) ;export
  (import (scheme base) (liii error))
  (begin
    (define (tree-depth tree)
      (if (tree-cyclic? tree)
        (value-error "tree-depth: tree is cyclic: ~S" tree)
        (let loop
          ((x tree))
          (cond ((not (pair? x)) 0)
                (else (+ 1
                        (let elt-loop
                          ((rest x) (max-d 0))
                          (cond ((not (pair? rest)) max-d)
                                (else (let ((d (if (pair? (car rest)) (loop (car rest)) 0)))
                                        (elt-loop (cdr rest) (max max-d d))
                                      ) ;let
                                ) ;else
                          ) ;cond
                        ) ;let
                      ) ;+
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
