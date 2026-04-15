(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; range->list
;; 将 range 转换为列表。
;;
;; 语法
;; ----
;; (range->list r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; list
;; 包含 range 所有元素的列表。
;;
;; 示例
;; ----
;; (range->list (numeric-range 0 5)) => '(0 1 2 3 4)
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 5)))
  (check (range->list r) => '(0 1 2 3 4))
) ;let


(let ((r (numeric-range 0 0)))
  (check (range->list r) => '())
) ;let


(let ((r (numeric-range 10 20 2)))
  (check (range->list r)
    =>
    '(10 12 14 16 18)
  ) ;check
) ;let


(let ((r (vector-range #(a b c d e))))
  (check (range->list r) => '(a b c d e))
) ;let


(let ((r (string-range "hello")))
  (check (range->list r)
    =>
    '(#\h #\e #\l #\l #\o)
  ) ;check
) ;let


(check-report)
