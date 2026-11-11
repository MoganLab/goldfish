;; base64url-decode
;; 将 RFC 4648 §5 定义的 URL 安全 Base64 字符串解码为字节向量。
;;
;; 语法
;; ----
;; (base64url-decode str)
;;
;; 参数
;; ----
;; str : string
;; 要解码的 Base64URL 字符串；'=' 填充可有可无，
;; 若存在必须恰好等于规范化填充量（与 Go encoding/base64、
;; 主流 JWT 实现的严格策略一致）。
;;
;; 返回值
;; ----
;; bytevector
;; 解码后的原始字节序列。
;;
;; 注意
;; ----
;; - 空字符串解码为空字节向量
;; - 只接受 base64url 字母表（A-Z a-z 0-9 - _），标准 Base64 的
;;   '+' '/' 属于非法字符
;;
;; 错误处理
;; ----
;; - 输入非 string -> type-error
;; - 含非法字符 -> value-error
;; - 正文长度模 4 余 1 -> value-error
;; - '=' 数量与规范化填充不符 -> value-error

(import (liii base64) (liii check) (liii string))

(check-set-mode! 'report-failed)

;; 空输入
(check (base64url-decode "") => #u8())

;; 标准示例（无填充形式）
(check (base64url-decode "Zg") => #u8(102))
(check (base64url-decode "Zm8") => #u8(102 111))
(check (base64url-decode "Zm9v") => #u8(102 111 111))
(check (base64url-decode "Zm9vYg") => #u8(102 111 111 98))
(check (base64url-decode "Zm9vYmE") => #u8(102 111 111 98 97))
(check (base64url-decode "Zm9vYmFy") => #u8(102 111 111 98 97 114))

;; 带规范化填充的输入同样被接受
(check (base64url-decode "Zg==") => #u8(102))
(check (base64url-decode "Zm8=") => #u8(102 111))
(check (base64url-decode "Zm9vYg==") => #u8(102 111 111 98))
(check (base64url-decode "Zm9vYmE=") => #u8(102 111 111 98 97))

;; base64url 字母表边界（- 与 _）
(check (base64url-decode "-w") => #u8(251))
(check (base64url-decode "_w") => #u8(255))
(check (base64url-decode "-_3-") => #u8(251 253 254))
(check (base64url-decode "____") => #u8(255 255 255))
(check (base64url-decode "AAAA") => #u8(0 0 0))
(check (base64url-decode "AA") => #u8(0))

;; 字符串载荷的解码（UTF-8 字节）
(check (utf8->string (base64url-decode "aGVsbG8")) => "hello")
(check (utf8->string (base64url-decode "6YeR6bG8")) => "金鱼")

;; 较长输入
(check (utf8->string (base64url-decode "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4"))
  =>
  "The quick brown fox jumps over the lazy dog."
) ;check

;; Round-trip 一致性
(check (base64url-decode (base64url-encode #u8(255 0 127 128))) => #u8(255 0 127 128))
(check (base64url-decode (base64url-encode #u8())) => #u8())

;; 非法字符：标准 Base64 字母表的 '+' '/' 不被接受
(check-catch 'value-error (base64url-decode "+/8="))
(check-catch 'value-error (base64url-decode "YWJ-@"))
(check-catch 'value-error (base64url-decode "!m9v"))
(check-catch 'value-error (base64url-decode "YWJ "))
(check-catch 'value-error (base64url-decode "YWJ\n"))

;; 长度非法：正文长度模 4 余 1
(check-catch 'value-error (base64url-decode "Y"))
(check-catch 'value-error (base64url-decode "Zm9vY"))

;; 填充数量错误
(check-catch 'value-error (base64url-decode "Zg="))
(check-catch 'value-error (base64url-decode "Zg==="))
(check-catch 'value-error (base64url-decode "===="))
(check-catch 'value-error (base64url-decode "=="))
(check-catch 'value-error (base64url-decode "="))

;; 填充位置错误
(check-catch 'value-error (base64url-decode "Zg=v"))

;; 类型错误
(check-catch 'type-error (base64url-decode #u8(90 103)))
(check-catch 'type-error (base64url-decode 123))
(check-catch 'type-error (base64url-decode 'sym))

(check-report)
