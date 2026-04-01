(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-ref
;; 访问可变长向量中指定位置的元素。
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
;; ----
;; any
;; 返回指定位置的元素。
;;
;; 错误处理
;; ----
;; bounds-error
;; 当索引越界时抛出。

(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-ref fv 0) => 'a)
  (check (flexvector-ref fv 1) => 'b)
  (check (flexvector-ref fv 2) => 'c)
) ;let

(let ((fv (flexvector 10 20 30)))
  (check (flexvector-ref fv 0) => 10)
  (check (flexvector-ref fv 2) => 30)
) ;let

(check-report)
