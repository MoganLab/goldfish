(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; codepoint->utf8
;; 将 Unicode 码点转换为 UTF-8 编码的字节向量。
;;
;; 语法
;; ----
;; (codepoint->utf8 codepoint)
;;
;; 参数
;; ----
;; codepoint : integer
;; Unicode 码点（0 到 #x10FFFF）。
;;
;; 返回值
;; ----
;; bytevector
;; UTF-8 编码的字节向量。
;;
;; 描述
;; ----
;; 编码规则：
;; - U+0000 到 U+007F: 1 字节编码
;; - U+0080 到 U+07FF: 2 字节编码
;; - U+0800 到 U+FFFF: 3 字节编码
;; - U+10000 到 U+10FFFF: 4 字节编码
;;
;; 错误处理
;; ----
;; value-error 当码点超出 Unicode 范围时。
;; type-error 当参数不是整数时。


;; 1 字节编码 (U+0000 到 U+007F)
(check (codepoint->utf8 72) => #u8(72))
(check (codepoint->utf8 0) => #u8(0))
(check (codepoint->utf8 127)
  =>
  #u8(127)
) ;check
(check (codepoint->utf8 65) => #u8(65))


;; 2 字节编码 (U+0080 到 U+07FF)
(check (codepoint->utf8 164)
  =>
  #u8(194 164)
) ;check
(check (codepoint->utf8 128)
  =>
  #u8(194 128)
) ;check
(check (codepoint->utf8 2047)
  =>
  #u8(223 191)
) ;check


;; 3 字节编码 (U+0800 到 U+FFFF)
(check (codepoint->utf8 20013)
  =>
  #u8(228 184 173)
) ;check
(check (codepoint->utf8 2048)
  =>
  #u8(224 160 128)
) ;check
(check (codepoint->utf8 65535)
  =>
  #u8(239 191 191)
) ;check


;; 4 字节编码 (U+10000 到 U+10FFFF)
(check (codepoint->utf8 128077)
  =>
  #u8(240 159 145 141)
) ;check
(check (codepoint->utf8 65536)
  =>
  #u8(240 144 128 128)
) ;check
(check (codepoint->utf8 1114111)
  =>
  #u8(244 143 191 191)
) ;check


;; 常见字符测试
(check (codepoint->utf8 128640)
  =>
  #u8(240 159 154 128)
) ;check
(check (codepoint->utf8 127881)
  =>
  #u8(240 159 142 137)
) ;check


;; 与 utf8->codepoint 互逆操作
(check (utf8->codepoint (codepoint->utf8 72))
  =>
  72
) ;check
(check (utf8->codepoint (codepoint->utf8 20013)
       ) ;utf8->codepoint
  =>
  20013
) ;check
(check (utf8->codepoint (codepoint->utf8 128077)
       ) ;utf8->codepoint
  =>
  128077
) ;check
(check (utf8->codepoint (codepoint->utf8 1114111)
       ) ;utf8->codepoint
  =>
  1114111
) ;check


;; 错误处理
(check-catch 'value-error
  (codepoint->utf8 -1)
) ;check-catch
(check-catch 'value-error
  (codepoint->utf8 1114112)
) ;check-catch
(check-catch 'type-error
  (codepoint->utf8 "not-an-integer")
) ;check-catch
(check-catch 'type-error
  (codepoint->utf8 #\A)
) ;check-catch


(check-report)
