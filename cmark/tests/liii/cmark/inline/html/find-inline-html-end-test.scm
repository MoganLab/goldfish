;; find-inline-html-end
;; 从 start 起查找行内 HTML 片段的结束位置。
;;
;; 语法
;; ----
;; (find-inline-html-end str start)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; 起始位置。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 找到返回结束位置，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline html))

(check-set-mode! 'report-failed)

(check (find-inline-html-end "<span>hi" 0) => 6)
(check (find-inline-html-end "<!-- x -->" 0) => 10)
(check (find-inline-html-end "<unclosed" 0) => #f)

(check-report)
