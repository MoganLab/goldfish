;; base64-decode
;; 根据输入类型将 Base64 字符串或字节向量解码为原始内容。
;;
;; 语法
;; ----
;; (base64-decode x)
;;
;; 参数
;; ----
;; x : string 或 bytevector
;; 要解码的 Base64 字符串或字节向量。
;;
;; 返回值
;; ----
;; string 或 bytevector
;; 返回与输入类型对应的解码结果。
;;
;; 注意
;; ----
;; - 空字符串解码为空字符串
;; - 支持带'='填充的 Base64 字符串
;;
;; 错误处理
;; ----
;; 输入类型不受支持时抛出 type-error；Base64 内容非法时抛出 value-error。

(import (liii base64) (liii check))

(check-set-mode! 'report-failed)

(check (base64-decode (string->utf8 "Zm9v")) => #u8(102 111 111))
(check-catch 'type-error (base64-decode 1))

(check (base64-decode "") => "")

(check (base64-decode "YQ==") => "a")
(check (base64-decode "eg==") => "z")
(check (base64-decode "Zg==") => "f")
(check (base64-decode "Zm8=") => "fo")
(check (base64-decode "Zm9v") => "foo")
(check (base64-decode "Zm9vYg==") => "foob")
(check (base64-decode "Zm9vYmE=") => "fooba")
(check (base64-decode "Zm9vYmFy") => "foobar")

(check-report)
