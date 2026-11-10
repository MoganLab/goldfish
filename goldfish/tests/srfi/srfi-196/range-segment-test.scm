;; range-segment
;; 将 range 分割为固定大小的段。
;;
;; 语法
;; ----
;; (range-segment r k)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;;
;; k : exact-positive-integer
;; 每段的大小。
;;
;; 返回值
;; ----
;; list
;; range 列表，每个 range 最多包含 k 个元素。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range，或 k 不是精确整数时抛出。
;; - value-error：当 k 为负或为零时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 10)))
  (let ((segs (range-segment r 3)))
    (check (length segs) => 4)
    (check (range-length (car segs)) => 3)
    (check (range-length (cadddr segs)) => 1)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((segs (range-segment r 5)))
    (check (length segs) => 2)
    (check (range-length (car segs)) => 5)
    (check (range-length (cadr segs)) => 5)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((segs (range-segment r 10)))
    (check (length segs) => 1)
    (check (range-length (car segs)) => 10)
  ) ;let
) ;let

;; k 大于 range 长度时返回单个元素段
(let ((r (numeric-range 0 3)))
  (let ((segs (range-segment r 10)))
    (check (length segs) => 1)
    (check (range-length (car segs)) => 3)
  ) ;let
) ;let

;; k 不是精确整数时抛出 type-error
(let ((r (numeric-range 0 10)))
  (check-catch 'type-error (range-segment r "3"))
  (check-catch 'type-error (range-segment r 1.5))
  (check-catch 'type-error (range-segment r 'k))
) ;let

;; k 为负或为零时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (range-segment r 0))
  (check-catch 'value-error (range-segment r -1))
) ;let

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-segment 42 3))
(check-catch 'type-error (range-segment "r" 3))


(check-report)
