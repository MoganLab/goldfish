;; parse-fence-line
;; 解析围栏代码块起始行。
;;
;; 语法
;; ----
;; (parse-fence-line line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; list 或 #f
;; 匹配返回 (char len indent info)，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check (parse-fence-line "```scheme") => '(#\` 3 0 "scheme"))
(check (parse-fence-line "``") => #f)

(check-report)
