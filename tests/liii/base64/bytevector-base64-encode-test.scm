(import (liii base64) (liii check))

(check-set-mode! 'report-failed)

;; bytevector-base64-encode
;; 将原始字节向量编码为 Base64 字节向量。
;;
;; 语法
;; ----
;; (bytevector-base64-encode bv)
;;
;; 参数
;; ----
;; bv : bytevector
;; 要编码的原始字节序列。
;;
;; 返回值
;; ----
;; bytevector
;; Base64 编码后的字节向量，遵循 RFC 4648：
;; - 每 3 个字节编码为 4 个字符
;; - 不足 3 字节时使用 '=' 填充
;; - 字符集为 A-Z, a-z, 0-9, '+', '/'
;; - 输出长度恒为 4 的倍数
;;
;; 注意
;; ----
;; - 空字节向量编码为空字节向量
;; - 输出字节值为 ASCII（0~127），不做 UTF-8 转换
;;
;; 错误处理
;; ----
;; - 输入非 bytevector -> type-error

;; 空输入
(check (bytevector-base64-encode #u8()) => #u8())

;; 标准 RFC 4648 示例（覆盖 1/2/3 字节填充情况）
(check (bytevector-base64-encode #u8(102)) => (string->utf8 "Zg=="))
(check (bytevector-base64-encode #u8(102 111)) => (string->utf8 "Zm8="))
(check (bytevector-base64-encode #u8(102 111 111)) => (string->utf8 "Zm9v"))
(check (bytevector-base64-encode #u8(102 111 111 98))
  =>
  (string->utf8 "Zm9vYg==")
) ;check
(check (bytevector-base64-encode #u8(102 111 111 98 97))
  =>
  (string->utf8 "Zm9vYmE=")
) ;check
(check (bytevector-base64-encode #u8(102 111 111 98 97 114))
  =>
  (string->utf8 "Zm9vYmFy")
) ;check

;; 边界字节（覆盖全部填充情况）
(check (bytevector-base64-encode #u8(0)) => (string->utf8 "AA=="))
(check (bytevector-base64-encode #u8(255)) => (string->utf8 "/w=="))
(check (bytevector-base64-encode #u8(0 0)) => (string->utf8 "AAA="))
(check (bytevector-base64-encode #u8(255 255)) => (string->utf8 "//8="))
(check (bytevector-base64-encode #u8(0 0 0)) => (string->utf8 "AAAA"))
(check (bytevector-base64-encode #u8(255 255 255)) => (string->utf8 "////"))

;; 字符集覆盖（+ / 边界）
(check (bytevector-base64-encode #u8(251 253 254)) => (string->utf8 "+/3+"))

;; 含 NUL 字节（不应被截断）
(check (bytevector-base64-encode #u8(0 0 0)) => (string->utf8 "AAAA"))

;; 较长输入
(check (bytevector-base64-encode (string->utf8 "The quick brown fox jumps over the lazy dog.")
       ) ;bytevector-base64-encode
  =>
  (string->utf8 "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4=")
) ;check

;; Round-trip 一致性
(check (bytevector-base64-decode (bytevector-base64-encode #u8())) => #u8())
(check (bytevector-base64-decode (bytevector-base64-encode #u8(1 2 3 4 5)))
  =>
  #u8(1 2 3 4 5)
) ;check
(check (bytevector-base64-decode (bytevector-base64-encode #u8(255 0 127 128)))
  =>
  #u8(255 0 127 128)
) ;check

;; 类型错误
(check-catch 'type-error (bytevector-base64-encode "foo"))
(check-catch 'type-error (bytevector-base64-encode 123))
(check-catch 'type-error (bytevector-base64-encode 'sym))

(check-report)
