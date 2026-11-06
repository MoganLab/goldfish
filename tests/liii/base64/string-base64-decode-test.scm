;; string-base64-decode
;; 将 Base64 字符串解码为原始 UTF-8 字符串。
;;
;; 语法
;; ----
;; (string-base64-decode str)
;;
;; 参数
;; ----
;; str : string
;; 要解码的 Base64 字符串。
;;
;; 返回值
;; ----
;; string
;; 解码后的 UTF-8 字符串。
;;
;; 错误处理
;; ----
;; 输入不是字符串时抛出 type-error；Base64 内容非法时抛出 value-error。

(import (liii base64) (liii check))

(check-set-mode! 'report-failed)

(check (string-base64-decode "") => "")
(check (string-base64-decode "Zg==") => "f")
(check (string-base64-decode "Zm8=") => "fo")
(check (string-base64-decode "Zm9v") => "foo")
(check (string-base64-decode "R29sZGZpc2g=") => "Goldfish")
(check (string-base64-decode "6YeR6bG8") => "金鱼")

(check-catch 'type-error (string-base64-decode (string->utf8 "Zm9v")))
(check-catch 'type-error (string-base64-decode 42))
(check-catch 'value-error (string-base64-decode "invalid"))

(check-report)
