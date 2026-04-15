(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf8->utf16be
;; 将 UTF-8 编码的字节向量转换为 UTF-16BE 编码的字节向量。
;;
;; 语法
;; ----
;; (utf8->utf16be bytevector)
;;
;; 参数
;; ----
;; bytevector : bytevector
;; UTF-8 编码的字节向量。
;;
;; 返回值
;; ----
;; bytevector
;; UTF-16BE 编码的字节向量。
;;
;; 错误处理
;; ----
;; value-error 当字节向量包含无效的 UTF-8 编码序列时。
;; type-error 当参数不是字节向量时。


;; ASCII 字符
(check (utf8->utf16be #u8(72))
  =>
  #u8(0 72)
) ;check
(check (utf8->utf16be #u8(72 101 108 108 111))
  =>
  #u8(0 72 0 101 0 108 0 108 0 111)
) ;check


;; 2 字节 UTF-8 -> 2 字节 UTF-16BE
(check (utf8->utf16be #u8(195 164))
  =>
  #u8(0 228)
) ;check


;; 3 字节 UTF-8 -> 2 字节 UTF-16BE
(check (utf8->utf16be #u8(228 184 173))
  =>
  #u8(78 45)
) ;check


;; 4 字节 UTF-8 -> 4 字节 UTF-16BE（代理对）
(check (utf8->utf16be #u8(240 159 145 141))
  =>
  #u8(216 61 220 77)
) ;check
(check (utf8->utf16be #u8(240 159 154 128))
  =>
  #u8(216 61 222 128)
) ;check


;; 空字节向量
(check (utf8->utf16be #u8()) => #u8())


;; 与 utf16be->utf8 互逆操作
(check (utf8->utf16be (utf16be->utf8 #u8(0 72))
       ) ;utf8->utf16be
  =>
  #u8(0 72)
) ;check
(check (utf8->utf16be (utf16be->utf8 #u8(78 45))
       ) ;utf8->utf16be
  =>
  #u8(78 45)
) ;check
(check (utf8->utf16be (utf16be->utf8 #u8(216 61 220 77))
       ) ;utf8->utf16be
  =>
  #u8(216 61 220 77)
) ;check


;; 错误处理
(check-catch 'value-error
  (utf8->utf16be #u8(255))
) ;check-catch
(check-catch 'type-error
  (utf8->utf16be "not-a-bytevector")
) ;check-catch


(check-report)
