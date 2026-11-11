;; sha256-bytes
;; 计算字符串或字节向量的 SHA256 哈希值，返回原始二进制摘要。
;;
;; 语法
;; ----
;; (sha256-bytes str-or-bv)
;;
;; 参数
;; ----
;; str-or-bv : string? 或 bytevector?
;; 要进行哈希计算的字符串（按其字节内容处理）或原始字节向量。
;;
;; 返回值
;; -----
;; bytevector?
;; 返回 32 字节的原始 SHA256 摘要字节向量。
;;
;; 说明
;; ----
;; 1. 与 (sha256 str) 计算同一哈希，但返回原始字节而非十六进制文本
;; 2. 适合需要二进制摘要的场景，如 Base64URL 编码生成 PKCE 的 code_challenge
;; 3. 输入为字符串时按字节内容哈希，UTF-8 多字节字符与
;;    (sha256-bytes (string->utf8 str)) 结果一致
;;
;; 错误处理
;; ----
;; - 输入既非字符串也非字节向量 -> type-error

(import (liii check) (liii hashlib))

(check-set-mode! 'report-failed)

;; ; 基本功能测试：常见字符串哈希（与 sha256 十六进制值一一对应）
(check (sha256-bytes "")
  =>
  #u8(227 176 196 66 152 252 28 20 154 251 244 200 153 111 185 36 39 174 65 228 100 155 147 76 164 149 153 27 120 82 184 85)
) ;check
(check (sha256-bytes "hello")
  =>
  #u8(44 242 77 186 95 176 163 14 38 232 59 42 197 185 226 158 27 22 30 92 31 167 66 94 115 4 51 98 147 139 152 36)
) ;check
(check (sha256-bytes "abc")
  =>
  #u8(186 120 22 191 143 1 207 234 65 65 64 222 93 174 34 35 176 3 97 163 150 23 122 156 180 16 255 97 242 0 21 173)
) ;check
(check
  (sha256-bytes "The quick brown fox jumps over the lazy dog")
  =>
  #u8(215 168 251 179 7 215 128 148 105 202 154 188 176 8 46 79 141 86 81 228 109 60 219 118 45 2 208 191 55 201 229 146)
) ;check

;; ; 返回值长度恒为 32 字节
(check (bytevector-length (sha256-bytes "")) => 32)
(check (bytevector-length (sha256-bytes "hello")) => 32)

;; ; 字节向量输入：与等价字符串输入结果一致
(check (sha256-bytes #u8(104 101 108 108 111)) => (sha256-bytes "hello"))
(check (sha256-bytes #u8()) => (sha256-bytes ""))

;; ; 字节向量输入：含 0 与 255 边界字节不被截断
(check (sha256-bytes #u8(0 255 128 0))
  =>
  #u8(241 27 24 101 152 69 181 194 126 96 64 162 133 134 187 231 157 183 194 77 78 218 121 48 56 17 81 189 70 2 73 13)
) ;check

;; ; UTF-8 多字节字符串与显式 utf8 编码输入一致
(check (sha256-bytes "中文") => (sha256-bytes #u8(228 184 173 230 150 135)))

;; ; 与 sha256 十六进制接口的一致性：摘要首字节与十六进制前两字符对应
(check
  (bytevector-u8-ref (sha256-bytes "hello") 0)
  =>
  44
) ;check

;; 异常情况测试：非法参数类型
(check-catch 'type-error (sha256-bytes #\a))
(check-catch 'type-error (sha256-bytes 123))
(check-catch 'type-error (sha256-bytes 'sym))

(check-report)
