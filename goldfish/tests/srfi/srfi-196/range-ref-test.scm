;; range-ref
;; 获取 range 中指定索引位置的元素。
;;
;; 语法
;; ----
;; (range-ref r index)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; index : exact-natural
;; 索引位置，从 0 开始。
;;
;; 返回值
;; ----
;; any
;; 索引位置的元素。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range，或 index 不是精确整数时抛出。
;; - value-error：当 index 超出 range 长度时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 10)))
  (check (range-ref r 0) => 0)
  (check (range-ref r 5) => 5)
  (check (range-ref r 9) => 9)
) ;let


(let ((r (numeric-range 10 30 2)))
  (check (range-ref r 0) => 10)
  (check (range-ref r 1) => 12)
  (check (range-ref r 9) => 28)
) ;let


(let ((r (range 10 (lambda (i) (* i 2)))))
  (check (range-ref r 0) => 0)
  (check (range-ref r 5) => 10)
  (check (range-ref r 9) => 18)
) ;let


(let ((r (vector-range #(a b c d e))))
  (check (range-ref r 0) => 'a)
  (check (range-ref r 4) => 'e)
) ;let

;; index 等于 range 长度时越界
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (range-ref r 10))
) ;let

;; index 不是精确整数时抛出 type-error
(let ((r (numeric-range 0 10)))
  (check-catch 'type-error (range-ref r 'a))
  (check-catch 'type-error (range-ref r "0"))
  (check-catch 'type-error (range-ref r 1.5))
  (check-catch 'type-error (range-ref r #f))
) ;let

;; index 为负数时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (range-ref r -1))
) ;let

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-ref 42 0))
(check-catch 'type-error (range-ref "r" 0))


(check-report)
