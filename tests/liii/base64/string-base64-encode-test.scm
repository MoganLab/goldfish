;; string-base64-encode
;; 将字符串的 UTF-8 字节编码为 Base64 字符串。
;;
;; 语法
;; ----
;; (string-base64-encode str)
;;
;; 参数
;; ----
;; str : string
;; 要编码的字符串。
;;
;; 返回值
;; ----
;; string
;; 符合 RFC 4648 的 Base64 编码字符串。
;;
;; 错误处理
;; ----
;; 输入不是字符串时抛出 type-error。

(import (liii base64) (liii check))

(check-set-mode! 'report-failed)

(check (string-base64-encode "") => "")
(check (string-base64-encode "f") => "Zg==")
(check (string-base64-encode "fo") => "Zm8=")
(check (string-base64-encode "foo") => "Zm9v")
(check (string-base64-encode "Goldfish") => "R29sZGZpc2g=")
(check (string-base64-encode "金鱼") => "6YeR6bG8")

(check-catch 'type-error (string-base64-encode #u8(102 111 111)))
(check-catch 'type-error (string-base64-encode 42))

(check-report)
