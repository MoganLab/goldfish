;; comparator-ordered?
;; 判断比较器是否支持排序（是否提供 ordering 过程）。
;;
;; 语法
;; ----
;; (comparator-ordered? comparator)
;;
;; 参数
;; ----
;; comparator : comparator
;; 返回值
;; ----
;; boolean
;; 比较器支持排序时返回 #t，否则返回 #f。
;; 注意
;; ----
;; eq/eqv/equal 比较器不提供 ordering，因此返回 #f。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (comparator-ordered? (make-default-comparator)))
(check-true (comparator-ordered? (make-comparator number? = < number-hash)))

(check-false (comparator-ordered? (make-eq-comparator)))
(check-false (comparator-ordered? (make-eqv-comparator)))
(check-false (comparator-ordered? (make-equal-comparator)))

(check-report)
