(define-library (hint liii e2e-app)
  (import (scheme base))
  (export hint-liii/e2e-app)
  (begin

    ;; e2e-app-fn : 原库本地实现，声明在此合法
    (define hint-e2e-app-fn '((any? -> any?)))

    ;; e2e-shared-fn : 实现在 (srfi srfi-1)，不应在再导出库中声明
    (define hint-e2e-shared-fn '((any? -> any?)))

    (define (hint-liii/e2e-app name)
      (case name
       ((e2e-app-fn) hint-e2e-app-fn)
       ((e2e-shared-fn) hint-e2e-shared-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
