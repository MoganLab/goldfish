;; count-leading-spaces
;; 统计字符串前导空格字符个数。
;;
;; 语法
;; ----
;; (count-leading-spaces str)
;;
;; 参数
;; ----
;; str : string
;; 目标字符串。
;;
;; 返回值
;; ----
;; integer
;; 返回前导空格数量。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (count-leading-spaces "   abc") => 3)
(check (count-leading-spaces "\tabc") => 0)

(check-report)
