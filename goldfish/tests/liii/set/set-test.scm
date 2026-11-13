;; set
;; 创建一个新的 set。
;;
;; 语法
;; ----
;; (set element ...)
;;
;; 参数
;; ----
;; element ... : any
;; 初始元素。
;;
;; 返回值
;; ----
;; set
;; 返回包含指定元素的 set。
;;
;; 错误处理
;; ----
;; 无异常抛出

(import (liii check) (liii set) (srfi srfi-128))

(check-set-mode! 'report-failed)

(check-true (set? (set 1 2 3)))
(check-true (set-contains? (set 1) 1))

;; 空集合
(define s-empty (set))
(check-true (set? s-empty))
(check-true (set-empty? s-empty))
(check (set-size s-empty) => 0)

;; 去重与默认比较器
(define s-dup (set 1 2 2 3 3 3))
(check (set-size s-dup) => 3)
(check-true (set=? s-dup (set 1 2 3)))
(check-true (eq? (set-element-comparator s-dup) (set-element-comparator s-empty)))

;; 字符串与符号元素
(define s-mixed (set "a" "b" "a"))
(check (set-size s-mixed) => 2)
(check-true (set-contains? s-mixed "a"))
(check-false (set-contains? s-mixed "c"))

;; 使用指定比较器构造
(define eq-comp (make-eq-comparator))
(define s-eq (make-set-with-comparator eq-comp 'a 'b 'a 'c))
(check (set-size s-eq) => 3)
(check-true (eq? (set-element-comparator s-eq) eq-comp))
(check-true (set-contains? s-eq 'a))

(check-report)
