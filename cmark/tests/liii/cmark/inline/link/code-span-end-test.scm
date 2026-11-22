;; code-span-end
;; 从反引号位置 pos 起跳过整个行内代码跨度并返回结束位置。
;;
;; 语法
;; ----
;; (code-span-end str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 反引号起始位置。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 闭合时返回代码跨度后位置，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link))

(check-set-mode! 'report-failed)

(check (code-span-end "`a`b" 0) => 3)

(check-report)
