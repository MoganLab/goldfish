;; 原库 import (liii dep)：my-assoc 返回 (or #f pair?)，
;; my-cdr 需要 pair?

(define-library (liii infercheck)
  (import (liii dep))
  (export bad-ref ok-unknown)
  (begin

    ;; my-cdr 需要 pair?，my-assoc 返回 (or #f pair?) -> 确定冲突
    (define (bad-ref bundle key)
      (my-cdr (my-assoc key bundle))
    ) ;define

    ;; 变量实参为 unknown，不报
    (define (ok-unknown x)
      (my-cdr (my-cdr x))
    ) ;define
  ) ;begin
) ;define-library
