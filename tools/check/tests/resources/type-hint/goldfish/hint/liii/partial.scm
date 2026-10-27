(define-library (hint liii partial)
  (import (scheme base))
  (export hint-liii/partial)
  (begin

    (define hint-f1
      '((any? -> any?))
    ) ;define

    ;; f2 故意不标记
    (define (hint-liii/partial name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
