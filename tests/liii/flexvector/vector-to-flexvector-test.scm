(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; vector->flexvector
;; 将普通向量（vector）转换为 flexvector。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (vector->flexvector vec)
;; (vector->flexvector vec start)
;; (vector->flexvector vec start end)
;;
;; 参数
;; ----
;; vec : vector
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
;; 返回新的 flexvector。
;;
;; 另见
;; ----
;; flexvector->vector - flexvector 转向量
;; list->flexvector - 列表转 flexvector

;; 基本转换
(check (flexvector->vector (vector->flexvector #(1 2 3))) => #(1 2 3))

;; 空向量
(check (flexvector->list (vector->flexvector #())) => '())

;; 从指定位置转换
(let ((vec #(1 2 3 4 5)))
  (check (flexvector->vector (vector->flexvector vec 2)) => #(3 4 5))
) ;let

;; 转换区间 [start, end)
(let ((vec #(1 2 3 4 5)))
  (check (flexvector->vector (vector->flexvector vec 1 4)) => #(2 3 4))
) ;let

;; 边界测试：空区间
(let ((vec #(1 2 3)))
  (check (flexvector->vector (vector->flexvector vec 0 0)) => #())
  (check (flexvector->vector (vector->flexvector vec 3 3)) => #())
) ;let

;; 单元素
(let ((vec #(only)))
  (check (flexvector->vector (vector->flexvector vec)) => #(only))
) ;let

;; 往返测试
(let ((vec #(a b c d e)))
  (check (flexvector->vector (vector->flexvector vec)) => vec)
) ;let

;; 转换后可修改
(let ((vec #(1 2 3))
      (fv #f))
  (set! fv (vector->flexvector vec))
  (flexvector-set! fv 0 999)
  (check (flexvector-ref fv 0) => 999)
  ;; 不影响原 vector
  (check vec => #(1 2 3))
) ;let

(check-report)
