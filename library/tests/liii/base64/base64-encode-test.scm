;; base64-encode
;; 根据输入类型将字符串或字节向量编码为 Base64。
;;
;; 语法
;; ----
;; (base64-encode x)
;;
;; 参数
;; ----
;; x : string 或 bytevector
;; 要编码的字符串或原始字节序列。
;;
;; 返回值
;; ----
;; string 或 bytevector
;; 返回与输入类型对应的 Base64 编码结果。遵循 RFC 4648 标准：
;; - 每3个字节编码为4个字符
;; - 不足3字节时使用'='填充
;; - 使用A-Z, a-z, 0-9, '+', '/'字符集
;;
;; 注意
;; ----
;; - 空字符串编码为空字符串
;; - 编码结果长度总是4的倍数（通过填充实现）
;; - 严格遵循Base64标准规范
;;
;; 错误处理
;; ----
;; 当输入不是字符串或字节向量时引发 type-error

(import (liii base64) (liii check) (liii error))

(check-set-mode! 'report-failed)

(check (base64-encode #u8(102 111 111)) => (string->utf8 "Zm9v"))

(check (base64-encode "") => "")
(check (base64-encode "a") => "YQ==")
(check (base64-encode "z") => "eg==")
(check (base64-encode "f") => "Zg==")
(check (base64-encode "fo") => "Zm8=")
(check (base64-encode "foo") => "Zm9v")
(check (base64-encode "foob") => "Zm9vYg==")
(check (base64-encode "fooba") => "Zm9vYmE=")
(check (base64-encode "foobar") => "Zm9vYmFy")

(check-catch 'type-error (base64-encode 1))

(check-report)
