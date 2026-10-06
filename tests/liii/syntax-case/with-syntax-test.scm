(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; with-syntax
;; 为局部表达式模式匹配绑定模式变量，以便在内部的 syntax 模板中引用。
;;
;; 语法
;; ----
;; (with-syntax ((pattern expr) ...) body ...)
;;
;; 参数
;; ----
;; ((pattern expr) ...) : bindings
;; 语法绑定列表，将各 expr 的求值结果分别按 pattern 模式解构绑定到模式变量。
;;
;; body ... : expressions
;; 在绑定作用域内求值的表达式序列。
;;
;; 返回值
;; -----
;; any
;; 最后一个 body 表达式的求值结果。

;; 1. 局部模式变量解构与计算
(check (with-syntax (((a b) '(10 20)))
         (quasisyntax (sum (unsyntax (+ (syntax a) (syntax b))))))
       => '(sum 30))

;; 2. 多个独立模式绑定
(check (with-syntax ((x 'hello)
                     (y 'world))
         (syntax (x y)))
       => '(hello world))

;; 3. 省略号与列表结构绑定
(check (with-syntax (((item ...) '(a b c)))
         (syntax (list item ...)))
       => '(list a b c))

(check-report)
