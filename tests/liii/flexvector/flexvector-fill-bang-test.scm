(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-fill!
;; 用指定值填充 flexvector 的指定区间。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-fill! fv fill)
;; (flexvector-fill! fv fill start)
;; (flexvector-fill! fv fill start end)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; fill : any
;;   填充值。
;;
;; start : exact-nonnegative-integer (可选，默认 0)
;;   起始索引（包含）。
;;
;; end : exact-nonnegative-integer (可选，默认长度)
;;   结束索引（不包含）。
;;
;; 返回值
;; -----
;; 返回值未指定。
;;
;; 副作用
;; -----
;; 修改 fv 指定区间的元素。
;;
;; 另见
;; ----
;; make-flexvector - 创建时填充
;; flexvector-set! - 设置单个元素

;; 填充整个向量
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'x)
  (check (flexvector->list fv) => '(x x x x x))
) ;let

;; 从指定位置填充到末尾
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'y 2)
  (check (flexvector->list fv) => '(1 2 y y y))
) ;let

;; 填充指定区间 [start, end)
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'z 1 3)
  (check (flexvector->list fv) => '(1 z z 4 5))
) ;let

;; 边界：空区间
(let ((fv (flexvector 1 2 3)))
  (flexvector-fill! fv 'x 1 1)
  (check (flexvector->list fv) => '(1 2 3))
) ;let

;; 边界：end 超出长度
(let ((fv (flexvector 1 2 3)))
  (flexvector-fill! fv 'x 0 10)
  (check (flexvector->list fv) => '(x x x))
) ;let

;; 边界：start 超出长度
(let ((fv (flexvector 1 2 3)))
  (flexvector-fill! fv 'x 5 10)
  (check (flexvector->list fv) => '(1 2 3))
) ;let

;; 单元素向量
(let ((fv (flexvector 'a)))
  (flexvector-fill! fv 'b)
  (check (flexvector->list fv) => '(b))
) ;let

;; 填充不同类型的值
(let ((fv (flexvector 1 2 3)))
  (flexvector-fill! fv #f)
  (check (flexvector->list fv) => '(#f #f #f))
) ;let

(check-report)
