;; percent-decode
;; 解码 URI 中的百分号转义序列。
;;
;; 语法
;; ----
;; (percent-decode value)
;;
;; 参数
;; ----
;; value : string
;; 待解码的字符串。
;;
;; 返回值
;; ----
;; string
;; 解码后的字符串。
;;
;; 注意
;; ----
;; 转义序列按 UTF-8 字节解码，因此 %E4%B8%AD 得到 "中"。
;;
;; 错误处理
;; ----
;; 转义序列不完整或包含非十六进制字符时抛出 json-schema-unresolvable。

(import (liii check) (liii json schema pointer))

(check-set-mode! 'report-failed)

;; ASCII 转义。

(check (percent-decode "%7E0%7E1") => "~0~1")
(check (percent-decode "%2F") => "/")
(check (percent-decode "%20") => " ")

;; 多字节 UTF-8 转义。

(check (percent-decode "%E4%B8%AD") => "中")

;; 没有转义时原样返回。

(check (percent-decode "abc") => "abc")
(check (percent-decode "") => "")

;; 非法转义时报错。

(check-catch 'json-schema-unresolvable (percent-decode "%"))
(check-catch 'json-schema-unresolvable (percent-decode "%XZ"))
(check-catch 'json-schema-unresolvable (percent-decode "abc%2"))

(check-report)
