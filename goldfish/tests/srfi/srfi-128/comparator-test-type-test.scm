;; comparator-test-type
;; 使用比较器判断对象是否满足类型约束。
;;
;; 语法
;; ----
;; (comparator-test-type comparator obj)
;;
;; 参数
;; ----
;; comparator : comparator
;; obj : any
;; 待检查的对象。
;; 返回值
;; ----
;; boolean
;; 对象满足比较器的类型约束时返回 #t，否则返回 #f。
;; 注意
;; ----
;; 不会抛出异常，可用于前置校验。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let ((comp (make-comparator number? = < number-hash)))
  (check (comparator-test-type comp 1) => #t)
  (check (comparator-test-type comp "str") => #f)
) ;let

;; 默认比较器接受任意对象
(let ((comp (make-default-comparator)))
  (check (comparator-test-type comp 1) => #t)
  (check (comparator-test-type comp "str") => #t)
) ;let

(check-report)
