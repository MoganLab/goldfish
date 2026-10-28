(define-library (hint liii a7reexport)
  (import (scheme base))
  (export hint-liii/a7reexport)
  (begin

    ;; own-fn : 原库本地实现，声明在此合法
    (define hint-own-fn '((any? -> any?)))

    ;; dep-fn : 再导出自 (liii a7dep)，不应在此声明
    (define hint-dep-fn '((any? -> any?)))

    (define (hint-liii/a7reexport name)
      (case name
       ((own-fn) hint-own-fn)
       ((dep-fn) hint-dep-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
