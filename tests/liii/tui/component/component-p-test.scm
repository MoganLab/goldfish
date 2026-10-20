(import (liii check)
        (liii tui component))

;; component?
;; 检查对象是否为 TUI 组件。
;;
;; 语法
;; ----
;; (component? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若对象是合法的 TUI Component 则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; component? 用于判断一个对象是否实现了 Component 协议。
;; 所有通过 make-component、make-text、make-truncated-text、make-spacer
;; 构造的组件均满足该谓词。非组件对象（如数字、字符串、列表）返回 #f。

(check (component? (make-component (lambda (comp width) (list "test")))) => #t)
(check (component? (make-text "hello")) => #t)
(check (component? (make-truncated-text "hello")) => #t)
(check (component? (make-spacer 2)) => #t)

(check (component? 123) => #f)
(check (component? "string") => #f)
(check (component? '()) => #f)
(check (component? '(1 2 3)) => #f)
(check (component? #f) => #f)
(check (component? (lambda () 1)) => #f)

(check-report)
