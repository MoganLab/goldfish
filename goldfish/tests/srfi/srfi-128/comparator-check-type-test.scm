;; comparator-check-type
;; 检查对象是否满足比较器的类型约束，不满足时抛出 type-error。
;;
;; 语法
;; ----
;; (comparator-check-type comparator obj)
;;
;; 参数
;; ----
;; comparator : comparator
;; obj : any
;; 待检查的对象。
;; 返回值
;; ----
;; boolean
;; 对象满足类型约束时返回 #t。
;; 注意
;; ----
;; 与 comparator-test-type 的区别在于不满足时抛出异常而非返回 #f。
;; 错误处理
;; ----
;; type-error 当对象不满足比较器的类型约束时抛出。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let ((comp (make-comparator number? = < number-hash)))
  (check (comparator-check-type comp 1) => #t)
  (check-catch 'type-error (comparator-check-type comp "str"))
) ;let

(check-report)
