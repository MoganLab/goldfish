;; normalize-list-item-line
;; 剥除列表项行首对应的内容缩进。
;;
;; 语法
;; ----
;; (normalize-list-item-line line . maybe-indent)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; string
;; 返回去缩进后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block list-collect))

(check-set-mode! 'report-failed)

(check (normalize-list-item-line "  abc" 2) => "abc")

(check-report)
