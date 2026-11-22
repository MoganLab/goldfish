;; parse-inline-link-rest
;; 从 ( 位置起解析行内链接的 (url title) 部分。
;;
;; 语法
;; ----
;; (parse-inline-link-rest str start)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; ( 字符位置。
;;
;; 返回值
;; ----
;; list 或 #f
;; 成功返回 (url title next-pos)，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link))

(check-set-mode! 'report-failed)

(check (parse-inline-link-rest "(https://a.b \"T\")" 1)
  =>
  '(("https://a.b" . "T") . 17)
) ;check

(check-report)
