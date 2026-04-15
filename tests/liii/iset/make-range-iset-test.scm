(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; make-range-iset
;; 创建一个包含整数范围的集合。
;;
;; 语法
;; ----
;; (make-range-iset start end)
;; (make-range-iset start end step)
;;
;; 参数
;; ----
;; start : exact-integer
;; 包含的起始值。
;;
;; end : exact-integer
;; 不包含的结束值。
;;
;; step : exact-integer (可选，默认为 1)
;; 步长值。可以为负数。
;;
;; 返回值
;; -----
;; 返回包含从 start 到 end（不包含）的整数序列的 iset。
;;
(check (iset->list (make-range-iset 25 30))
  =>
  '(25 26 27 28 29)
) ;check
(check (iset->list (make-range-iset -10 10 6))
  =>
  '(-10 -4 2 8)
) ;check
(check (iset->list (make-range-iset 10 -10 -6))
  =>
  '(-8 -2 4 10)
) ;check


(check-report)
