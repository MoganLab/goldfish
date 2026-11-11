;; base64url-encode
;; 将字节向量编码为 RFC 4648 §5 定义的 URL 安全 Base64 字符串。
;;
;; 语法
;; ----
;; (base64url-encode bv)
;;
;; 参数
;; ----
;; bv : bytevector
;; 要编码的原始字节序列。
;;
;; 返回值
;; ----
;; string
;; 符合 RFC 4648 §5 的 Base64URL 字符串：
;; - 字符集为 A-Z, a-z, 0-9, '-', '_'（用 '-' '_' 取代 '+' '/'）
;; - 不含 '=' 填充（遵循 §3.2 省略填充，与 JWT RFC 7515、
;;   PKCE RFC 7636 的惯例一致）
;;
;; 注意
;; ----
;; - 空字节向量编码为空字符串
;; - 编码结果可直接嵌入 URL、文件名，无需百分号转义
;;
;; 错误处理
;; ----
;; - 输入非 bytevector -> type-error

(import (liii base64) (liii check) (liii string))

(check-set-mode! 'report-failed)

;; 空输入
(check (base64url-encode #u8()) => "")

;; RFC 4648 示例对应的 base64url 结果（覆盖 1/2/3 字节填充情况）
(check (base64url-encode #u8(102)) => "Zg")
(check (base64url-encode #u8(102 111)) => "Zm8")
(check (base64url-encode #u8(102 111 111)) => "Zm9v")
(check (base64url-encode #u8(102 111 111 98)) => "Zm9vYg")
(check (base64url-encode #u8(102 111 111 98 97)) => "Zm9vYmE")
(check (base64url-encode #u8(102 111 111 98 97 114)) => "Zm9vYmFy")

;; 与标准 Base64 的差异：+ -> -，/ -> _
(check (base64url-encode #u8(251)) => "-w")
(check (base64url-encode #u8(255)) => "_w")
(check (base64url-encode #u8(251 253 254)) => "-_3-")
(check (base64url-encode #u8(255 254)) => "__4")

;; 边界字节
(check (base64url-encode #u8(0)) => "AA")
(check (base64url-encode #u8(0 0)) => "AAA")
(check (base64url-encode #u8(0 0 0)) => "AAAA")
(check (base64url-encode #u8(255 255 255)) => "____")

;; 字符串载荷：先取 UTF-8 字节再编码
(check (base64url-encode (string->utf8 "hello")) => "aGVsbG8")
(check (base64url-encode (string->utf8 "Goldfish")) => "R29sZGZpc2g")
(check (base64url-encode (string->utf8 "金鱼")) => "6YeR6bG8")

;; 较长输入
(check (base64url-encode (string->utf8 "The quick brown fox jumps over the lazy dog."))
  =>
  "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4"
) ;check

;; 输出不含填充与标准字母表专属字符
(let ((s (base64url-encode (string->utf8 "foob"))))
  (check (string-contains? s "=") => #f)
  (check (string-contains? s "+") => #f)
  (check (string-contains? s "/") => #f)
) ;let

;; Round-trip 一致性
(check (base64url-decode (base64url-encode #u8())) => #u8())
(check (base64url-decode (base64url-encode #u8(1 2 3 4 5))) => #u8(1 2 3 4 5))
(check (base64url-decode (base64url-encode #u8(255 0 127 128))) => #u8(255 0 127 128))

;; 类型错误
(check-catch 'type-error (base64url-encode "foo"))
(check-catch 'type-error (base64url-encode 123))
(check-catch 'type-error (base64url-encode 'sym))

(check-report)
