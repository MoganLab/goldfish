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
          (if (not (pair? x))
            0
            (+ 1
              (let elt-loop
                ((rest x) (max-d 0))
                (if (not (pair? rest))
                  max-d
                  (elt-loop (cdr rest) (max max-d (loop (car rest))))
                ) ;if
              ) ;let
            ) ;+
          ) ;if
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
