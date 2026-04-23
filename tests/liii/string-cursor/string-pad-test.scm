(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-pad
;; 在字符串左侧补齐字符，使其达到指定长度。
;;
;; 语法
;; ----
;; (string-pad s len [char start end])
;;
;; 参数
;; ----
;; s : string
;; 要填充的字符串
;;
;; len : integer
;; 目标长度
;;
;; char : character (可选)
;; 填充字符，默认为 #\space
;;
;; start : integer (可选)
;; 子串起始位置，默认为0
;;
;; end : integer (可选)
;; 子串结束位置，默认为字符串字符数
;;
;; 返回值
;; ------
;; string?
;; 填充后的字符串

(check (string-pad "325" 5) => "  325")
(check (string-pad "71325" 5) => "71325")
(check (string-pad "8871325" 5) => "71325")
(check (string-pad "" 5) => "     ")
(check (string-pad "325" 5 #\*) => "**325")
(check (string-pad "abc" 6 #\- 0) => "---abc")

;; 测试中文字符
(check (string-pad "中文" 5) => "   中文")
(check (string-pad "中文测试" 3) => "文测试")

;; string-pad-right
;; 在字符串右侧补齐字符，使其达到指定长度。
(check (string-pad-right "325" 5) => "325  ")
(check (string-pad-right "71325" 5) => "71325")
(check (string-pad-right "8871325" 5) => "88713")
(check (string-pad-right "" 5) => "     ")
(check (string-pad-right "325" 5 #\*) => "325**")

;; 测试中文字符
(check (string-pad-right "中文" 5) => "中文   ")
(check (string-pad-right "中文测试" 3) => "中文测")

(check-report)
