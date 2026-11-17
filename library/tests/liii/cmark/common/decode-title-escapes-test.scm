;; decode-title-escapes
;; 解码链接 title 中的实体引用与反斜杠转义。
;;
;; 语法
;; ----
;; (decode-title-escapes str)
;;
;; 参数
;; ----
;; str : string
;; 原始 title 字符串。
;;
;; 返回值
;; ----
;; string
;; 返回解码后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark common))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (decode-title-escapes "a\\*b &amp; c") => "a*b & c")

(check-report)
