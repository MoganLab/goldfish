;; normalize-bullet-list-item-line
;; 剥除无序列表项行的默认缩进。
;;
;; 语法
;; ----
;; (normalize-bullet-list-item-line line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; string
;; 返回规范化行。

(import (scheme base))
(import (liii check))
(import (liii cmark block list-collect))

(check-set-mode! 'report-failed)

(check (normalize-bullet-list-item-line "  abc") => "abc")

(check-report)
