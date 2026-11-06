;; set
;; 使用指定比较器创建一个新的 set。
;;
;; 语法
;; ----
;; (set comparator element ...)
;;
;; 参数
;; ----
;; comparator : comparator
;; 用于判定元素等价性的比较器。
;;
;; element ... : any
;; 初始元素。
;;
;; 返回值
;; ----
;; set
;; 返回包含指定元素、并使用 comparator 作为比较器的 set。
;;
;; 错误处理
;; ----
;; 无异常抛出

(import (liii check) (srfi srfi-113) (srfi srfi-128))

(check-set-mode! 'report-failed)

(define comp (make-default-comparator))


(define s-empty (set comp))

(check-true (set? s-empty))
(check-true (set-empty? s-empty))


(define s-1-2-3 (set comp 1 2 3))

(check-true (set-contains? s-1-2-3 1))
(check-true (set-contains? s-1-2-3 3))
(check-false (set-contains? s-1-2-3 4))
(check (set-size s-1-2-3) => 3)

;; 重复元素会被去重

(define s-dup (set comp 1 2 2))

(check (set-size s-dup) => 2)

;; 比较器被保留

(check-true (eq? (set-element-comparator s-1-2-3) comp))

(check-report)
