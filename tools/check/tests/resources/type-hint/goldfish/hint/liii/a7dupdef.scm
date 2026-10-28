(define-library (hint liii a7dupdef)
  (import (scheme base))
  (export hint-liii/a7dupdef)
  (begin

    ;; 同一 hint 变量被 define 两次
    (define hint-f1 '((any? -> any?)))
    (define hint-f1 '((any? -> any?)))

    (define (hint-liii/a7dupdef name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
