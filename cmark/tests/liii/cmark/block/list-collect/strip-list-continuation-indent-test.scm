;; strip-list-continuation-indent
;; 剥除列表续行的缩进前缀。
;;
;; 语法
;; ----
;; (strip-list-continuation-indent line . maybe-indent)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; string
;; 返回剥除缩进后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block list-collect))

(check-set-mode! 'report-failed)

(check (strip-list-continuation-indent "    abc" 4) => "abc")

(check-report)
