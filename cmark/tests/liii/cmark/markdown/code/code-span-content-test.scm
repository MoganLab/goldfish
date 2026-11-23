;; code-span-content
;; 计算行内代码输出内容：首尾是空格或反引号时两侧各补一个空格。
;;
;; 语法
;; ----
;; (code-span-content literal)
;;
;; 参数
;; ----
;; literal : string
;; 行内代码字面内容。
;;
;; 返回值
;; ----
;; string
;; 填充后的输出内容。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown code))

(check-set-mode! 'report-failed)

(check-true (procedure? code-span-content))

(check (code-span-content "gf test") => "gf test")
(check (code-span-content "") => "")
(check (code-span-content " a ") => "  a  ")
(check (code-span-content "a ") => " a  ")
(check (code-span-content " a") => "  a ")
(check (code-span-content "`x`") => " `x` ")
(check (code-span-content "a`") => " a` ")

(check-report)
