(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; codepoint->utf16be
;; 将 Unicode 码点转换为 UTF-16BE 编码的字节向量。
;;
;; 语法
;; ----
;; (codepoint->utf16be codepoint)
;;
;; 参数
;; ----
;; codepoint : integer
;; Unicode 码点（0 到 #x10FFFF）。
;;
;; 返回值
;; ----
;; bytevector
;; UTF-16BE 编码的字节向量。
;;
;; 描述
;; ----
;; 编码规则：
;; - U+0000 到 U+FFFF: 2 字节编码（大端序）
;; - U+10000 到 U+10FFFF: 4 字节编码（代理对，大端序）
;;
;; 错误处理
;; ----
;; value-error 当码点超出 Unicode 范围或处于代理区时。
;; type-error 当参数不是整数时。


;; 基本 BMP 字符（2 字节）
(check (codepoint->utf16be 72)
  =>
  #u8(0 72)
) ;check
(check (codepoint->utf16be 65)
  =>
  #u8(0 65)
) ;check
(check (codepoint->utf16be 164)
  =>
  #u8(0 164)
) ;check
(check (codepoint->utf16be 20013)
  =>
  #u8(78 45)
) ;check


;; 边界测试
(check (codepoint->utf16be 0)
  =>
  #u8(0 0)
) ;check
(check (codepoint->utf16be 65535)
  =>
  #u8(255 255)
) ;check


;; 代理区外字符（4 字节）
(check (codepoint->utf16be 128077)
  =>
  #u8(216 61 220 77)
) ;check
(check (codepoint->utf16be 128640)
  =>
  #u8(216 61 222 128)
) ;check
(check (codepoint->utf16be 65536)
  =>
  #u8(216 0 220 0)
) ;check
(check (codepoint->utf16be 1114111)
  =>
  #u8(219 255 223 255)
) ;check


;; 与 utf16be->codepoint 互逆操作
(check (utf16be->codepoint (codepoint->utf16be 72)
       ) ;utf16be->codepoint
  =>
  72
) ;check
(check (utf16be->codepoint (codepoint->utf16be 20013)
       ) ;utf16be->codepoint
  =>
  20013
) ;check
(check (utf16be->codepoint (codepoint->utf16be 128077)
       ) ;utf16be->codepoint
  =>
  128077
) ;check
(check (utf16be->codepoint (codepoint->utf16be 1114111)
       ) ;utf16be->codepoint
  =>
  1114111
) ;check


;; 错误处理
(check-catch 'value-error
  (codepoint->utf16be -1)
) ;check-catch
(check-catch 'value-error
  (codepoint->utf16be 1114112)
) ;check-catch
(check-catch 'value-error
  (codepoint->utf16be 55296)
) ;check-catch
(check-catch 'value-error
  (codepoint->utf16be 57343)
) ;check-catch
(check-catch 'type-error
  (codepoint->utf16be "not-an-integer")
) ;check-catch


(check-report)
