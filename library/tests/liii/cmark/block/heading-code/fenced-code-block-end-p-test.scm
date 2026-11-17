;; fenced-code-block-end?
;; 判断行是否为闭合指定围栏的结束行。
;;
;; 语法
;; ----
;; (fenced-code-block-end? fence-char fence-len line)
;;
;; 参数
;; ----
;; fence-char : char
;; 围栏字符。
;; fence-len : integer
;; 最小围栏长度。
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; boolean
;; 是闭合行返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check-true (fenced-code-block-end? #\` 3 "```"))
(check-false (fenced-code-block-end? #\` 4 "```"))

(check-report)
