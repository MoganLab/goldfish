;; range-reverse
;; 反转 range 的元素顺序。
;;
;; 语法
;; ----
;; (range-reverse r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; range
;; 新的 range 对象，元素顺序反转。
;;
;; 错误处理
;; ----
;; - type-error：当任一参数不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 5)))
  (let ((rev (range-reverse r)))
    (check (range->list rev) => '(4 3 2 1 0))
  ) ;let
) ;let


(let ((r (numeric-range 0 3)))
  (check (range->list (range-reverse r)) => '(2 1 0))
) ;let


(let ((r (numeric-range 0 1)))
  (check (range->list (range-reverse r)) => '(0))
) ;let


(let ((r (numeric-range 0 0)))
  (check (range->list (range-reverse r)) => '())
) ;let


(let ((r (vector-range #(a b c d e))))
  (check (range->list (range-reverse r)) => '(e d c b a))
) ;let



;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-reverse 42))
(check-catch 'type-error (range-reverse "r"))
(check-catch 'type-error (range-reverse '(0 1 2)))

(check-report)
