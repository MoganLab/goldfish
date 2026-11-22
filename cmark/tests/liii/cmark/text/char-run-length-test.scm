;; char-run-length
;; 从 start 起统计连续出现字符 ch 的长度。
;;
;; 语法
;; ----
;; (char-run-length str start ch)
;;
;; 参数
;; ----
;; str : string
;; 目标字符串。
;; start : integer
;; 起始索引。
;; ch : char
;; 目标字符。
;;
;; 返回值
;; ----
;; integer
;; 返回连续字符个数。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (char-run-length "### title" 0 #\#) => 3)
(check (char-run-length "abc" 0 #\#) => 0)

(check-report)
