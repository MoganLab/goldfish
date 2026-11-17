;; strip-markdown-list-indent
;; 剥除列表项行首对应的标记缩进。
;;
;; 语法
;; ----
;; (strip-markdown-list-indent line)
;;
;; 参数
;; ----
;; line : string
;; 列表项行。
;;
;; 返回值
;; ----
;; string
;; 返回去缩进后的行。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (strip-markdown-list-indent "  - a") => "- a")

(check-report)
