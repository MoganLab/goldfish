(define-library (hint liii a7dupreg)
  (import (scheme base))
  (export hint-liii/a7dupreg)
  (begin

    (define hint-f1 '((any? -> any?)))
    (define hint-f2 '((any? -> any?)))

    ;; 同一函数名 f1 被登记两次
    (define (hint-liii/a7dupreg name)
      (case name
       ((f1) hint-f1)
       ((f1) hint-f2)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
