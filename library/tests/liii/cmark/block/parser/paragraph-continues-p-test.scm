;; paragraph-continues?
;; 判断文本行 line 是否可以作为当前段落的延续。
;;
;; 语法
;; ----
;; (paragraph-continues? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 可以延续段落返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block parser))

(check-set-mode! 'report-failed)

(check-true (paragraph-continues? "plain text"))
(check-false (paragraph-continues? "# heading"))
(check-false (paragraph-continues? ""))

(check-report)
