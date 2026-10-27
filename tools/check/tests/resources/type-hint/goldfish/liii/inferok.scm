;; 合法调用：my-cons 返回 pair? 喂 my-cdr；unknown 实参不报

(define-library (liii inferok)
  (import (liii dep))
  (export good1 good2)
  (begin

    (define (good1 x)
      (my-cdr (my-cons 1 2))
    ) ;define

    ;; 字面量与 unknown 实参均不报
    (define (good2 x)
      (my-cons 'a x)
    ) ;define
  ) ;begin
) ;define-library
