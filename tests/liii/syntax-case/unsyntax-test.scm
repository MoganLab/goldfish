(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; unsyntax
;; 在 quasisyntax 准引用语法模板内部求值表达式，并将结果嵌入模板。
;;
;; 语法
;; ----
;; (unsyntax expr)
;;
;; 参数
;; ----
;; expr : any
;; 待求值的表达式。
;;
;; 返回值
;; -----
;; any
;; 表达式的计算结果。

;; 1. 基本求值嵌入
(check (quasisyntax (foo (unsyntax 100))) => '(foo 100))

;; 2. 算术计算求值嵌入
(check (quasisyntax (value: (unsyntax (+ 10 20)))) => '(value: 30))

;; 3. 符号与字符串嵌入
(check (quasisyntax (name: (unsyntax (string->symbol "my-var")))) => '(name: my-var))

(check-report)
