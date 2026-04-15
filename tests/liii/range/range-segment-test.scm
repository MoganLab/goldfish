(import (liii check) (liii range))


(check-set-mode! 'report-failed)


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
;; 示例
;; ----
;; (range-segment (numeric-range 0 10) 3) => 4 个 range：[0,1,2], [3,4,5], [6,7,8], [9]
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 10)))
  (let ((segs (range-segment r 3)))
    (check (length segs) => 4)
    (check (range-length (car segs)) => 3)
    (check (range-length (cadddr segs))
      =>
      1
    ) ;check
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


(check-report)
