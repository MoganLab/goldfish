(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector->list
;; 将可变长向量转换为列表。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector->list fv)
;; (flexvector->list fv start)
;; (flexvector->list fv start end)
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
;; 返回列表，元素顺序与向量中相同。
;;
;; 另见
;; ----
;; list->flexvector - 列表转向量

;; 基本转换
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->list fv) => '(1 2 3)))

;; 空向量
(check (flexvector->list (flexvector)) => '())

;; 从指定位置转换
(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector->list fv 2) => '(3 4 5)))

;; 转换区间 [start, end)
(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector->list fv 1 4) => '(2 3 4)))

;; 边界测试
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->list fv 0 0) => '())
  (check (flexvector->list fv 3 3) => '()))

;; 单元素
(let ((fv (flexvector 'only)))
  (check (flexvector->list fv) => '(only)))

;; 往返测试
(let ((lst '(a b c d e)))
  (check (flexvector->list (list->flexvector lst)) => lst))

(check-report)
