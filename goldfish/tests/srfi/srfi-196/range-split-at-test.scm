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
;; 错误处理
;; ----
;; - type-error：当 r 不是 range，或 index 不是精确整数时抛出。
;; - value-error：当 index 为负或超出 range 长度时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




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

;; index 不是精确整数时抛出 type-error
(let ((r (numeric-range 0 10)))
  (check-catch 'type-error (range-split-at r 'a))
  (check-catch 'type-error (range-split-at r "4"))
  (check-catch 'type-error (range-split-at r 1.5))
) ;let

;; index 为负或超出 range 长度时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (range-split-at r -1))
  (check-catch 'value-error (range-split-at r 11))
) ;let

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-split-at 42 1))
(check-catch 'type-error (range-split-at "r" 1))


(check-report)
