(import (liii check) (liii goldfmt-format))

(check-set-mode! 'report-failed)

;; 测试 ''(a b) 应被格式化为 ''(a b)，而不是 '(quote (a b)) 或 '(#_quote (a b))
(check (format-string "''(a b)") => "''(a b)\n")

;; 测试列表内部的 quote 形式应格式化为 'x
(check (format-string "(list '(a b))") => "(list '(a b))\n")

(check-report)
