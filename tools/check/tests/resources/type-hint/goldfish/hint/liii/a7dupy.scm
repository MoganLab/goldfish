(define-library (hint liii a7dupy)
  (import (scheme base))
  (export hint-liii/a7dupy)
  (begin

    ;; 与 (hint liii a7dupx) 重复声明
    (define hint-dup-fn '((any? -> any?)))

    (define (hint-liii/a7dupy name)
      (case name
       ((dup-fn) hint-dup-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
