(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; syntax->datum
;; 递归剥除语法对象中的所有词法作用域和句法闭包包装，还原为干净的原生 S 表达式数据。
;;
;; 语法
;; ----
;; (syntax->datum stx)
;;
;; 参数
;; ----
;; stx : syntax / any
;; 语法对象或任意 S 表达式。
;;
;; 返回值
;; -----
;; any
;; 剥除所有句法包装后的纯净原生数据。

;; 1. 还原符号与列表
(check (syntax->datum (syntax (a b c))) => '(a b c))

;; 2. 还原标量值
(check (syntax->datum (syntax 123)) => 123)
(check (syntax->datum (syntax "hello")) => "hello")

;; 3. 还原向量与嵌套列表
(check (syntax->datum (syntax #(1 (2 3) 4))) => '#(1 (2 3) 4))

(check-report)
