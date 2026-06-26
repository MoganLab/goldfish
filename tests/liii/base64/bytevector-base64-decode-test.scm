(import (liii base64) (liii check))

(check-set-mode! 'report-failed)

;; bytevector-base64-decode
;; 将 Base64 编码的字节向量解码为原始字节向量。
;;
;; 语法
;; ----
;; (bytevector-base64-decode bv)
;;
;; 参数
;; ----
;; bv : bytevector
;; 要解码的 Base64 字节向量（长度必须是 4 的倍数）。
;;
;; 返回值
;; ----
;; bytevector
;; 解码后的原始字节序列。
;;
;; 注意
;; ----
;; - 空字节向量解码为空字节向量
;; - 支持 1~2 个 '=' 填充
;; - 输入字节可以是 0~255 任意值，结果字节向量保持原始字节，不做 UTF-8 转换
;;
;; 错误处理
;; ----
;; - 长度不是 4 的倍数 -> value-error
;; - 含非法字符（含空白、URL-safe 字符） -> value-error
;; - 填充位置错误 -> value-error
;; - 输入非 bytevector -> type-error

;; 空输入
(check (bytevector-base64-decode #u8()) => #u8())

;; 标准示例（与字符串入口一致）
(check (bytevector-base64-decode (string->utf8 "Zm9v")) => #u8(102 111 111))
(check (bytevector-base64-decode (string->utf8 "Zm9vYmFy"))
  =>
  #u8(102 111 111 98 97 114)
) ;check

;; 各种长度（1~6 字节）覆盖全部填充情况
(check (bytevector-base64-decode (string->utf8 "AA==")) => #u8(0))
(check (bytevector-base64-decode (string->utf8 "AQ==")) => #u8(1))
(check (bytevector-base64-decode (string->utf8 "/w==")) => #u8(255))
(check (bytevector-base64-decode (string->utf8 "AAA=")) => #u8(0 0))
(check (bytevector-base64-decode (string->utf8 "//8=")) => #u8(255 255))
(check (bytevector-base64-decode (string->utf8 "AAAA")) => #u8(0 0 0))
(check (bytevector-base64-decode (string->utf8 "AAAB")) => #u8(0 0 1))
(check (bytevector-base64-decode (string->utf8 "AAAAAA==")) => #u8(0 0 0 0))
(check (bytevector-base64-decode (string->utf8 "AAAAAAA=")) => #u8(0 0 0 0 0))
(check (bytevector-base64-decode (string->utf8 "AAAAAAAB")) => #u8(0 0 0 0 0 1))

;; 标准 Base64 字符集边界
(check (bytevector-base64-decode (string->utf8 "AAAA")) => #u8(0 0 0))
(check (bytevector-base64-decode (string->utf8 "////")) => #u8(255 255 255))
(check (bytevector-base64-decode (string->utf8 "+/3+")) => #u8(251 253 254))

;; 含 NUL 字节的解码（不应被截断）
(check (bytevector-base64-decode (string->utf8 "AAAA")) => #u8(0 0 0))

;; 较长输入
(check (bytevector-base64-decode (string->utf8 "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4=")
       ) ;bytevector-base64-decode
  =>
  #u8(84
    104
    101
    32
    113
    117
    105
    99
    107
    32
    98
    114
    111
    119
    110
    32
    102
    111
    120
    32
    106
    117
    109
    112
    115
    32
    111
    118
    101
    114
    32
    116
    104
    101
    32
    108
    97
    122
    121
    32
    100
    111
    103
    46
) ;#
) ;check

;; Round-trip 一致性
(check (bytevector-base64-decode (bytevector-base64-encode #u8(1 2 3 4 5)))
  =>
  #u8(1 2 3 4 5)
) ;check
(check (bytevector-base64-decode (bytevector-base64-encode #u8())) => #u8())
(check (bytevector-base64-decode (bytevector-base64-encode #u8(255 0 127 128)))
  =>
  #u8(255 0 127 128)
) ;check

;; 非法输入：长度不是 4 的倍数
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "Zg")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "Zm9")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "Zm9vYg")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "Zm9vYg===")))

;; 非法字符（含空白、URL-safe 字符 '-' '_' 等）
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "!m9v")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "Zm9v@")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "YWJ-")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "YWJ_")))

;; 填充位置错误
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "=Zm9")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "Zg=v")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "====")))
(check-catch 'value-error (bytevector-base64-decode (string->utf8 "Y===")))

;; 类型错误
(check-catch 'type-error (bytevector-base64-decode 123))
(check-catch 'type-error (bytevector-base64-decode "Zm9v"))
(check-catch 'type-error (bytevector-base64-decode 'sym))

(check-report)
