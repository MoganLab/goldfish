(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; range-split-at
;; 在指定位置将 range 分割为两部分。
;;
;; 语法
;; ----
;; (range-split-at r index)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;;
;; index : exact-natural
;; 分割位置。
;;
;; 返回值
;; ----
;; range, range
;; 两个值：前半部分和后半部分的 range。
;;
;; 示例
;; ----
;; (range-split-at (numeric-range 0 10) 4) => 两个 range：0-3 和 4-9
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 10)))
  (let-values (((left right) (range-split-at r 4)))
    (check (range-length left) => 4)
    (check (range-length right) => 6)
    (check (range-ref left 3) => 3)
    (check (range-ref right 0) => 4)
  ) ;let-values
) ;let


(let ((r (numeric-range 0 10)))
  (let-values (((left right) (range-split-at r 0)))
    (check (range-length left) => 0)
    (check (range-length right) => 10)
    (check (range-ref right 0) => 0)
  ) ;let-values
) ;let


(let ((r (numeric-range 0 10)))
  (let-values (((left right) (range-split-at r 10)))
    (check (range-length left) => 10)
    (check (range-length right) => 0)
    (check (range-ref left 9) => 9)
  ) ;let-values
) ;let


(check-report)
