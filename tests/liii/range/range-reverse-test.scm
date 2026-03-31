(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

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
;; 示例
;; ----
;; (range->list (range-reverse (numeric-range 0 5))) => '(4 3 2 1 0)
;;
;; 错误处理
;; ----
;; 无

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

(check-report)
