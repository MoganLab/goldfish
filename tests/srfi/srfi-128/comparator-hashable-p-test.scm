;; comparator-hashable?
;; 判断比较器是否支持哈希（是否提供 hash 过程）。
;;
;; 语法
;; ----
;; (comparator-hashable? comparator)
;;
;; 参数
;; ----
;; comparator : comparator
;; 返回值
;; ----
;; boolean
;; 比较器支持哈希时返回 #t，否则返回 #f。
;; 注意
;; ----
;; make-comparator 的 hash 参数为 #f 时表示不支持哈希。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (comparator-hashable? (make-default-comparator)))
(check-true (comparator-hashable? (make-eq-comparator)))

(check-false (comparator-hashable? (make-comparator number? = < #f)))

(check-report)
