;; code-span-delimiter
;; 计算行内代码的安全反引号定界符。
;;
;; 语法
;; ----
;; (code-span-delimiter literal)
;;
;; 参数
;; ----
;; literal : string
;; 行内代码字面内容。
;;
;; 返回值
;; ----
;; string
;; 长度为 max(1, 内容最长反引号游程 + 1) 的反引号串。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown code))

(check-set-mode! 'report-failed)

(check-true (procedure? code-span-delimiter))

(check (code-span-delimiter "gf test") => "`")
(check (code-span-delimiter "") => "`")
(check (code-span-delimiter "a`b") => "``")
(check (code-span-delimiter "a``b") => "```")
(check (code-span-delimiter "`x`") => "``")

(check-report)
