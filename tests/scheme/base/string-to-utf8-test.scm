(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string->utf8
;; 将字符串转换为 UTF-8 字节向量。
;;
;; 语法
;; ----
;; (string->utf8 string [start [end]])
;;
;; 参数
;; ----
;; string : string?
;; 待转换的字符串。
;; start : exact-nonnegative-integer?
;; 可选，起始字符索引，默认为 0。
;; end : exact-nonnegative-integer?
;; 可选，结束字符索引，默认为字符串长度。
;;
;; 返回值
;; ------
;; bytevector?
;; UTF-8 编码的字节向量。
;;
;; 说明
;; ----
;; 1. 空字符串返回空字节向量
;; 2. 按 UTF-8 编码转换每个字符
;; 3. ASCII 字符编码为单字节
(check (string->utf8 "") => #u8())
(check (string->utf8 "a") => #u8(97))
(check (string->utf8 "Hello")
  =>
  #u8(72 101 108 108 111)
) ;check
(check (string->utf8 "世界")
  =>
  #u8(228 184 150 231 149 140)
) ;check
(check (string->utf8 "abc" 1)
  =>
  #u8(98 99)
) ;check
(check (string->utf8 "abc" 0 2)
  =>
  #u8(97 98)
) ;check
(check-catch 'type-error
  (string->utf8)
) ;check-catch
(check-catch 'type-error
  (string->utf8 '())
) ;check-catch
(check-catch 'out-of-range
  (string->utf8 "abc" -1)
) ;check-catch

(check-report)
