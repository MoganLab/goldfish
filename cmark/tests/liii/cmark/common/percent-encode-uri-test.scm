;; percent-encode-uri
;; 对 URI 字符串中的非保留字符进行百分号编码。
;;
;; 语法
;; ----
;; (percent-encode-uri str . extra-encode-chars)
;;
;; 参数
;; ----
;; str : string
;; 原始 URI 字符串。
;;
;; 返回值
;; ----
;; string
;; 返回编码后的 URI 字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark common))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (percent-encode-uri "https://a.b/c d") => "https://a.b/c%20d")

(check-report)
