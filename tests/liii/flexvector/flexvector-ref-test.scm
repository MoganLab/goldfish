(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-ref
;; 访问可变长向量中的元素。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-ref fv index)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量。
;;
;; index : exact-nonnegative-integer
;;   元素索引，从 0 开始。
;;
;; 返回值
;; -----
;; 返回指定位置的元素。
;;
;; 错误
;; ----
;; 索引越界或负数索引会抛出错误。
;;
;; 示例
;; ----
;; ;; 基本访问
;; (flexvector-ref (flexvector 'a 'b 'c) 0) => 'a
;; (flexvector-ref (flexvector 'a 'b 'c) 2) => 'c
;;
;; ;; 访问第一个和最后一个元素
;; (define fv (flexvector 10 20 30))
;; (flexvector-ref fv 0)                      => 10  ; 第一个
;; (flexvector-ref fv (- (flexvector-length fv) 1)) => 30  ; 最后一个

(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-ref fv 0) => 'a)
  (check (flexvector-ref fv 1) => 'b)
  (check (flexvector-ref fv 2) => 'c))

;; 边界测试：单元素
(let ((fv (flexvector 'only)))
  (check (flexvector-ref fv 0) => 'only))

;; 边界测试：索引计算
(let ((fv (flexvector 10 20 30 40 50)))
  (check (flexvector-ref fv (- (flexvector-length fv) 1)) => 50))

(check-report)
