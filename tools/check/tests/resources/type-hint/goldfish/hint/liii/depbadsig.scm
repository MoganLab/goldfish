(define-library (hint liii depbadsig)
  (import (scheme base))
  (export hint-liii/depbadsig)
  (begin

    ;; 签名语法违规：缺 ->
    (define hint-g1 '((any? any?)))

    (define (hint-liii/depbadsig name)
      (case name
       ((g1) hint-g1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
