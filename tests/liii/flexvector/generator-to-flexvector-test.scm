(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; generator->flexvector
;; 将生成器转换为可变长向量。
;;
;; 语法
;; ----
;; (generator->flexvector gen)
;;
;; 参数
;; ----
;; gen : function
;; 生成器函数。
;;
;; 返回值
;; ----
;; flexvector
;; 包含生成器所有元素的向量。
;;
;; 描述
;; ----
;; 从生成器读取所有元素直到 eof，构建 flexvector。

(let ((genlist '(a b c)))
  (define (mock-generator)
    (if (pair? genlist)
      (let ((value (car genlist)))
        (set! genlist (cdr genlist))
        value
      ) ;let
      (eof-object)
    ) ;if
  ) ;define
  (check (flexvector->list (generator->flexvector mock-generator))
         => '(a b c)
  ) ;check
) ;let

(let ((count 0))
  (define (counting-gen)
    (if (< count 5)
      (let ((v count))
        (set! count (+ count 1))
        v
      ) ;let
      (eof-object)
    ) ;if
  ) ;define
  (check (flexvector->vector (generator->flexvector counting-gen))
         => #(0 1 2 3 4)
  ) ;check
) ;let

(check-report)
