(import (liii check)
        (liii goldfix))

(check-set-mode! 'report-failed)

;; parentheses-balanced?
;; 判断结构括号是否平衡，忽略字符串、注释、字符字面量和 raw string 中的括号。

(check (parentheses-balanced? "(define x 1)") => #t)

(check (parentheses-balanced? "(define x 1") => #f)

(check (parentheses-balanced? "(define x 1))") => #f)

(check (parentheses-balanced? "(display \")\") ; )") => #t)

(check (parentheses-balanced? "#\\)") => #t)

(check (parentheses-balanced? "(display #\"END\")END\")") => #t)

(check-report)
