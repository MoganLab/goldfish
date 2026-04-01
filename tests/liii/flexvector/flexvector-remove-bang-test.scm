(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-remove!
;; 从可变长向量中移除指定位置的元素。
;;
;; 语法
;; ----
;; (flexvector-remove! fv index)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 要移除元素的索引。
;;
;; 返回值
;; ----
;; any
;; 返回被移除的元素。
;;
;; 错误处理
;; ----
;; bounds-error
;; 当索引越界时抛出。

(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove! fv 1) => 'b)
  (check (flexvector-length fv) => 2)
  (check (flexvector-ref fv 1) => 'c)
  (check (flexvector->list fv) => '(a c))
) ;let

(let ((fv (flexvector 'x)))
  (check (flexvector-remove! fv 0) => 'x)
  (check (flexvector-empty? fv) => #t)
) ;let

(check-report)
