;; set-element-comparator
;; 获取 set 使用的比较器。
;;
;; 语法
;; ----
;; (set-element-comparator set)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; 返回值
;; ----
;; comparator
;; 返回该 set 使用的比较器。

(import (liii check) (srfi srfi-113) (srfi srfi-128))

(check-set-mode! 'report-failed)

(define comp (make-default-comparator))


(define s-empty (set comp))

(define s-1-2 (set comp 1 2))


(check-true (eq? (set-element-comparator s-empty) comp))
(check-true (eq? (set-element-comparator s-1-2) comp))

;; 不同比较器的 set 拥有不同的比较器

(define other-comp (make-equal-comparator))

(define s-other (set other-comp 1 2))

(check-false (eq? (set-element-comparator s-other) (set-element-comparator s-1-2)))

(check-report)
