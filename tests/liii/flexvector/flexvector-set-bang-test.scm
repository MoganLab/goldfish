(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-set!
;; 设置可变长向量中指定位置的元素。
;;
;; 语法
;; ----
;; (flexvector-set! fv index value)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 元素索引。
;;
;; value : any
;; 新值。
;;
;; 返回值
;; ----
;; any
;; 返回原来的值。
;;
;; 错误处理
;; ----
;; bounds-error
;; 当索引越界时抛出。

(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-set! fv 1 'd) => 'b)
  (check (flexvector-ref fv 1) => 'd)
  (check (flexvector->list fv) => '(a d c))
) ;let

(let ((fv (flexvector 10 20 30)))
  (flexvector-set! fv 0 100)
  (check (flexvector-ref fv 0) => 100)
) ;let

(check-report)
