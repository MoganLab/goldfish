;; parentheses-balanced?
;; 判断结构括号是否平衡，忽略字符串、注释、字符字面量和 raw string 中的括号。
;;
;; 语法
;; ----
;; (parentheses-balanced? source)
;;
;; 参数
;; ----
;; source : string? 源码文本
;;
;; 返回值
;; ------
;; boolean? 括号平衡时为 #t，否则为 #f

(import (liii check) (liii goldfix))

(check-set-mode! 'report-failed)

(check (parentheses-balanced? "(define x 1)") => #t)

(check (parentheses-balanced? "(define x 1") => #f)

(check (parentheses-balanced? "(define x 1))") => #f)

(check (parentheses-balanced? "(display \")\") ; )") => #t)

(check (parentheses-balanced? "#\\)") => #t)

(check (parentheses-balanced? "(display #\"END\")END\")") => #t)

(check-report)
