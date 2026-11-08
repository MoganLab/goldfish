;; list->set
;; 使用指定比较器将列表转换为 set。
;;
;; 语法
;; ----
;; (list->set comparator list)
;;
;; 参数
;; ----
;; comparator : comparator
;; 用于判定元素等价性的比较器。
;;
;; list : list
;; 要转换的列表。
;;
;; 返回值
;; ----
;; set
;; 返回包含列表中所有元素、并使用 comparator 作为比较器的 set。
;;
;; 注意
;; ----
;; 列表中的重复元素会被去重。
;;
;; 错误处理
;; ----
;; 无异常抛出

(import (liii check) (srfi srfi-113) (srfi srfi-128))

(check-set-mode! 'report-failed)

(define comp (make-default-comparator))


(define s-empty (set comp))

(define s-1-2-3 (set comp 1 2 3))

(define s-1-2 (set comp 1 2))


(define s-list-1 (list->set comp '(1 2 3)))

(check-true (set? s-list-1))
(check-true (eq? (set-element-comparator s-list-1) comp))
(check-true (set=? s-1-2-3 s-list-1))
(check-false (eq? s-1-2-3 s-list-1))


(define s-list-empty (list->set comp '()))

(check-true (set=? s-empty s-list-empty))
(check (set-size s-list-empty) => 0)


;; 列表中的重复元素会被去重

(define s-list-dup (list->set comp '(1 2 2 1)))

(check-true (set=? s-1-2 s-list-dup))
(check (set-size s-list-dup) => 2)

(check-report)
