;; range
;; 使用指定的长度和索引器函数创建 range。
;;
;; 语法
;; ----
;; (range length indexer)
;;
;; 参数
;; ----
;; length : exact-natural
;; range 的长度，必须是非负整数。
;;
;; indexer : procedure
;; 接受一个索引参数并返回对应位置的值的函数。
;;
;; 返回值
;; ----
;; range
;; 新创建的 range 对象。
;;
;; 注意
;; ----
;; 创建一个惰性序列，元素通过索引器函数按需计算。
;;
;; 错误处理
;; ----
;; - type-error：当 length 不是精确整数，或 indexer 不是过程时抛出。
;; - value-error：当 length 是负整数时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (range 10 (lambda (i) (* i 2)))))
  (check-true (range? r))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 5) => 10)
  (check (range-ref r 9) => 18)
) ;let

;; 长度为 0 的空 range 合法
(let ((r (range 0 (lambda (i) i))))
  (check-true (range? r))
  (check (range-length r) => 0)
) ;let

;; length 类型非法时抛出 type-error
(check-catch 'type-error (range 'a (lambda (i) i)))
(check-catch 'type-error (range "10" (lambda (i) i)))
(check-catch 'type-error (range 10.5 (lambda (i) i)))
(check-catch 'type-error (range #f (lambda (i) i)))
(check-catch 'type-error (range '(1 2) (lambda (i) i)))

;; length 为负数时抛出 value-error
(check-catch 'value-error (range -1 (lambda (i) i)))
(check-catch 'value-error (range -10 (lambda (i) i)))

;; indexer 不是过程时抛出 type-error
(check-catch 'type-error (range 10 'not-a-procedure))
(check-catch 'type-error (range 10 42))


(check-report)
