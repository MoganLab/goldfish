(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-swap!
;; 交换 flexvector 中两个位置的元素。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-swap! fv i j)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; i, j : exact-nonnegative-integer
;;   要交换的两个位置的索引。
;;
;; 返回值
;; -----
;; 返回值未指定。
;;
;; 副作用
;; -----
;; 交换 fv 中索引 i 和 j 处的元素。
;;
;; 另见
;; ----
;; flexvector-reverse! - 反转

;; 基本交换
(let ((fv (flexvector 10 20 30)))
  (flexvector-swap! fv 0 2)
  (check (flexvector->list fv) => '(30 20 10)))

;; 交换相邻元素
(let ((fv (flexvector 'a 'b 'c 'd)))
  (flexvector-swap! fv 1 2)
  (check (flexvector->list fv) => '(a c b d)))

;; 交换同一个位置（无变化）
(let ((fv (flexvector 1 2 3)))
  (flexvector-swap! fv 1 1)
  (check (flexvector->list fv) => '(1 2 3)))

;; 多次交换
(let ((fv (flexvector 1 2 3 4)))
  (flexvector-swap! fv 0 3)
  (flexvector-swap! fv 1 2)
  (check (flexvector->list fv) => '(4 3 2 1)))

;; 双元素向量
(let ((fv (flexvector 'x 'y)))
  (flexvector-swap! fv 0 1)
  (check (flexvector->list fv) => '(y x)))

;; 交换首尾
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-swap! fv 0 4)
  (check (flexvector->list fv) => '(5 2 3 4 1)))

(check-report)
