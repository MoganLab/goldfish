(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; bytevector-advance-utf8
;; 将字节向量中的索引向前推进一个 UTF-8 字符。
;;
;; 语法
;; ----
;; (bytevector-advance-utf8 bytevector index)
;; (bytevector-advance-utf8 bytevector index end)
;;
;; 参数
;; ----
;; bytevector : bytevector?
;; 包含 UTF-8 编码字节的向量。
;; index : exact-nonnegative-integer?
;; 当前字节索引。
;; end : exact-nonnegative-integer?
;; 可选，结束索引，默认为字节向量长度。
;;
;; 返回值
;; ------
;; exact-nonnegative-integer?
;; 下一个字符的起始字节索引；如果序列不完整或无效，则返回当前索引。
;;
;; 说明
;; ----
;; 1. 支持 1~4 字节的 UTF-8 序列
;; 2. 到达 end 时返回 end
;; 3. 遇到不完整或无效序列时返回当前索引
;; 4. ASCII 字符（0x00~0x7f）前进 1 字节
(check (bytevector-advance-utf8 #u8() 0) => 0)
(check (bytevector-advance-utf8 #u8(97) 0) => 1)
(check (bytevector-advance-utf8 #u8(97 98 99) 0) => 1)
(check (bytevector-advance-utf8 #u8(97 98 99) 1) => 2)
(check (bytevector-advance-utf8 #u8(97 98 99) 2) => 3)
(check (bytevector-advance-utf8 #u8(97 98 99) 3) => 3)
(check (bytevector-advance-utf8 #u8(228 184 150 231 149 140) 0)
  => 3
) ;check
(check (bytevector-advance-utf8 #u8(228 184 150 231 149 140) 3)
  => 6
) ;check
(check (bytevector-advance-utf8 #u8(240 159 152 128) 0)
  => 4
) ;check
(check (bytevector-advance-utf8 #u8(228 184) 0)
  => 0
) ;check
(check (bytevector-advance-utf8 #u8(255) 0)
  => 0
) ;check
(check (bytevector-advance-utf8 #u8(97 98) 0 1)
  => 1
) ;check
(check (bytevector-advance-utf8 #u8(97 98) 1 1)
  => 1
) ;check

(check-report)
