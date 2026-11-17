;; skip-html-or-autolink
;; 从 < 位置起跳过合法的尖括号 autolink 或行内 HTML。
;;
;; 语法
;; ----
;; (skip-html-or-autolink str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; < 字符位置。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 匹配时返回结束位置，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link))

(check-set-mode! 'report-failed)

(check (skip-html-or-autolink "<a>b" 0) => 3)

(check-report)
