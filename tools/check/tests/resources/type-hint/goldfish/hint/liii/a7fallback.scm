(define-library (hint liii a7fallback)
  (import (scheme base))
  (export hint-liii/a7fallback)
  (begin

    ;; 实现库 (liii a7fallbackdep) 没有 hint 库，允许在此声明
    (define hint-fb-fn '((any? -> any?)))

    (define (hint-liii/a7fallback name)
      (case name
       ((fb-fn) hint-fb-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
