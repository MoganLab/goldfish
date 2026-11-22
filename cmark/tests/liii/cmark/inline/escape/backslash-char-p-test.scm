;; backslash-char?
;; 判断位置 pos 处是否为反斜杠字符。
;;
;; 语法
;; ----
;; (backslash-char? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 位置索引。
;;
;; 返回值
;; ----
;; boolean
;; 是反斜杠返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline escape))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (backslash-char? "\\*" 0))
(check-false (backslash-char? "abc" 0))

(check-report)
