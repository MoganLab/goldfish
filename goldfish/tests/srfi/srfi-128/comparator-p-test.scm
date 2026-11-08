;; comparator?
;; 判断对象是否为比较器。
;;
;; 语法
;; ----
;; (comparator? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;; 返回值
;; ----
;; boolean
;; obj 是比较器时返回 #t，否则返回 #f。
;; 注意
;; ----
;; 比较器由 make-comparator 及其派生构造函数创建。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(check-true (comparator? (make-default-comparator)))
(check-true (comparator? (make-eq-comparator)))
(check-true (comparator? (make-eqv-comparator)))
(check-true (comparator? (make-equal-comparator)))

(check-false (comparator? 1))
(check-false (comparator? "str"))
(check-false (comparator? '()))
(check-false (comparator? #f))

(check-report)
