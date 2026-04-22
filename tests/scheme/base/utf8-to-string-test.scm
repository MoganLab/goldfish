(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; utf8->string
;; 将 UTF-8 字节向量转换为字符串。
;;
;; 语法
;; ----
;; (utf8->string bytevector [start [end]])
;;
;; 参数
;; ----
;; bytevector : bytevector?
;; UTF-8 编码的字节向量。
;; start : exact-nonnegative-integer?
;; 可选，起始索引，默认为 0。
;; end : exact-nonnegative-integer?
;; 可选，结束索引，默认为字节向量长度。
;;
;; 返回值
;; ------
;; string?
;; 解码后的字符串。
;;
;; 说明
;; ----
;; 1. 输入必须是有效的 UTF-8 序列
;; 2. 无效的 UTF-8 序列会引发错误
;; 3. 空字节向量返回空字符串
(check (utf8->string #u8()) => "")
(check (utf8->string #u8(97)) => "a")
(check (utf8->string #u8(72 101 108 108 111))
  =>
  "Hello"
) ;check
(check (utf8->string #u8(228 184 150 231 149 140)
       ) ;utf8->string
  =>
  "世界"
) ;check
(check (utf8->string #u8(97 98 99) 1)
  =>
  "bc"
) ;check
(check (utf8->string #u8(97 98 99) 0 2)
  =>
  "ab"
) ;check
(check-catch 'wrong-type-arg
  (utf8->string)
) ;check-catch
(check (utf8->string '()) => "")
(check-catch 'value-error
  (utf8->string #u8(255))
) ;check-catch

(check-report)
