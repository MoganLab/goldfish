(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; generator->flexvector
;; 生成器转换为可变长向量。
;;
;; 语法
;; ----
;; (generator->flexvector gen)
;;
(let ((genlist '(a b c)))
  (define (mock-generator)
    (if (pair? genlist)
      (let ((value (car genlist)))
        (set! genlist (cdr genlist))
        value)
      (eof-object)))
  (check (flexvector->list (generator->flexvector mock-generator))
         => '(a b c)))

(check-report)
