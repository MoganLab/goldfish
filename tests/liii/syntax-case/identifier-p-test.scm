(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; identifier?
;; 判断给定对象是否为标识符（符号或语法闭包标识符）。
;;
;; 语法
;; ----
;; (identifier? x)
;;
;; 参数
;; ----
;; x : any
;; 待检查的对象。
;;
;; 返回值
;; -----
;; boolean
;; 若 x 为标识符则返回 #t，否则返回 #f。

;; 1. 基础类型判定
(check-true (identifier? 'a))
(check-true (identifier? 'foo-bar))
(check-false (identifier? 123))
(check-false (identifier? "str"))
(check-false (identifier? '(a b)))
(check-false (identifier? '#(a b)))
(check-false (identifier? #t))
(check-false (identifier? '()))

(check-report)
