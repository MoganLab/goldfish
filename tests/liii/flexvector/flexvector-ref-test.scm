(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-ref
;; 访问可变长向量中的元素。
;;
;; 语法
;; ----
;; (flexvector-ref fv index)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 元素索引，从 0 开始。
;;
;; 返回值
;; -----
;; 返回指定位置的元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-ref fv 1) => 'b))

(check-report)
