;; inline-code-start?
;; 判断位置 pos 是否为行内代码反引号起始。
;;
;; 语法
;; ----
;; (inline-code-start? str pos)
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
;; 是反引号起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(check-true (inline-code-start? "`code`" 0))
(check-false (inline-code-start? "text" 0))

(check-report)
