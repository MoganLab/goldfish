(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector->vector
;; 将 flexvector 转换为普通向量（vector）。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector->vector fv)
;; (flexvector->vector fv start)
;; (flexvector->vector fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;;   源向量。
;;
;; start : exact-nonnegative-integer (可选，默认 0)
;;   起始索引（包含）。
;;
;; end : exact-nonnegative-integer (可选，默认长度)
;;   结束索引（不包含）。
;;
;; 返回值
;; -----
;; 返回新的 vector，包含指定范围的元素。
;;
;; 另见
;; ----
;; vector->flexvector - 向量转 flexvector

;; 基本转换
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector fv) => #(1 2 3))
) ;let

;; 空向量
(let ((fv (flexvector)))
  (check (flexvector->vector fv) => #())
) ;let

;; 从指定位置转换
(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector->vector fv 2) => #(3 4 5))
) ;let

;; 转换区间 [start, end)
(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector->vector fv 1 4) => #(2 3 4))
) ;let

;; 边界测试：空区间
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector fv 0 0) => #())
  (check (flexvector->vector fv 3 3) => #())
) ;let

;; 单元素
(let ((fv (flexvector 'only)))
  (check (flexvector->vector fv) => #(only))
) ;let

;; 往返测试
(let ((vec #(a b c d e)))
  (check (flexvector->vector (vector->flexvector vec)) => vec)
) ;let

;; 修改原向量不影响已转换的 vector
(let ((fv (flexvector 1 2 3))
      (vec #f))
  (set! vec (flexvector->vector fv))
  (flexvector-set! fv 0 999)
  (check vec => #(1 2 3))
) ;let

(check-report)
