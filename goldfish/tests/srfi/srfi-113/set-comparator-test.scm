;; set-comparator
;; 用于比较 set 对象本身的 comparator。
;;
;; 语法
;; ----
;; set-comparator
;;
;; 参数
;; ----
;; 无参数（导出为 comparator 对象）。
;;
;; 返回值
;; ----
;; comparator
;; 返回用于比较 set 对象的 SRFI-128 comparator，支持类型测试（set?）、
;; 相等性判断（set=?）与哈希计算，不支持全序比较（comparator-ordered? 返回 #f）。

(import (liii check) (liii set) (srfi srfi-128))

(check-set-mode! 'report-failed)

(check-true (comparator? set-comparator))
(check-false (comparator-ordered? set-comparator))
(check-true (comparator-hashable? set-comparator))

(define s1 (set 1 2 3))
(define s2 (set 3 2 1))
(define s3 (set 1 2 4))

(check-true (comparator-test-type set-comparator s1))
(check-false (comparator-test-type set-comparator '(1 2 3)))

(check-true (=? set-comparator s1 s2))
(check-false (=? set-comparator s1 s3))

(check (comparator-hash set-comparator s1) => (comparator-hash set-comparator s2))

;; 集合嵌套：使用 set-comparator 构造集合的集合
(define set-of-sets (make-set-with-comparator set-comparator s1 s2 s3))
(check (set-size set-of-sets) => 2)
(check-true (set-contains? set-of-sets (set 2 1 3)))
(check-false (set-contains? set-of-sets (set 9)))

(check-report)
