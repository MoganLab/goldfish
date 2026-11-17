;; read-html-tag-name
;; 从 start 起读取 HTML 标签名，返回结束位置或 #f。
;;
;; 语法
;; ----
;; (read-html-tag-name str start)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; 起始位置。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 合法标签名返回结束位置，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark common))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (read-html-tag-name "<div class=x>" 1) => 4)
(check (read-html-tag-name "<123>" 1) => #f)

(check-report)
