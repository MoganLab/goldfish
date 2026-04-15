(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf16be->codepoint
;; 将 UTF-16BE 编码的字节向量转换为 Unicode 码点。
;;
;; 语法
;; ----
;; (utf16be->codepoint bytevector)
;;
;; 参数
;; ----
;; bytevector : bytevector
;; UTF-16BE 编码的字节向量。
;;
;; 返回值
;; ----
;; integer
;; Unicode 码点。
;;
;; 描述
;; ----
;; 解码规则：
;; - 2 字节：直接解码为 BMP 字符
;; - 4 字节：解析代理对，解码为辅助平面字符
;;
;; 错误处理
;; ----
;; value-error 当字节向量无效时。
;; type-error 当参数不是字节向量时。


;; 基本 BMP 字符（2 字节）
(check (utf16be->codepoint #u8(0 72))
  =>
  72
) ;check
(check (utf16be->codepoint #u8(0 65))
  =>
  65
) ;check
(check (utf16be->codepoint #u8(0 164))
  =>
  164
) ;check
(check (utf16be->codepoint #u8(78 45))
  =>
  20013
) ;check


;; 边界测试
(check (utf16be->codepoint #u8(0 0))
  =>
  0
) ;check
(check (utf16be->codepoint #u8(255 255))
  =>
  65535
) ;check


;; 代理对（4 字节）
(check (utf16be->codepoint #u8(216 61 220 77))
  =>
  128077
) ;check
(check (utf16be->codepoint #u8(216 61 222 128))
  =>
  128640
) ;check
(check (utf16be->codepoint #u8(216 0 220 0))
  =>
  65536
) ;check
(check (utf16be->codepoint #u8(219 255 223 255)
       ) ;utf16be->codepoint
  =>
  1114111
) ;check


;; 与 codepoint->utf16be 互逆操作
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
  (utf16be->codepoint #u8())
) ;check-catch
(check-catch 'value-error
  (utf16be->codepoint #u8(0))
) ;check-catch
(check-catch 'value-error
  (utf16be->codepoint #u8(216 0))
) ;check-catch
(check-catch 'value-error
  (utf16be->codepoint #u8(220 0 220 0))
) ;check-catch
(check-catch 'type-error
  (utf16be->codepoint "not-a-bytevector")
) ;check-catch
(check-catch 'type-error
  (utf16be->codepoint 123)
) ;check-catch


(check-report)
