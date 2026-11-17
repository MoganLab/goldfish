;; decode-backslash-escapes-in-string
;; 解码字符串中的全部反斜杠转义。
;;
;; 语法
;; ----
;; (decode-backslash-escapes-in-string str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; 返回值
;; ----
;; string
;; 返回解码后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark inline escape))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (decode-backslash-escapes-in-string "a\\*b\\ c") => "a*b\\ c")

(check-report)
