;; parse-atx-heading-line
;; 解析 ATX 标题行，返回 (level . content) 或 #f。
;;
;; 语法
;; ----
;; (parse-atx-heading-line line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; pair 或 #f
;; 合法标题返回 (level . content)，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check (parse-atx-heading-line "## Hi ##") => '(2 . "Hi ##"))
(check (parse-atx-heading-line "####### too many") => #f)

(check-report)
