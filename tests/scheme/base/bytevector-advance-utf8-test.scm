;; bytevector-advance-utf8
;; 将字节向量中的索引向前推进一个 UTF-8 字符。
;;
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
;; ----
;; exact-nonnegative-integer?
;; 下一个字符的起始字节索引；如果序列不完整或无效，则返回当前索引。
;;
;; 说明
;; ----
;; 1. 支持 1~4 字节的 UTF-8 序列
;; 2. 到达 end 时返回 end
;; 3. 遇到不完整或无效序列时返回当前索引
;; 4. ASCII 字符（0x00~0x7f）前进 1 字节

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (bytevector-advance-utf8 #u8() 0) => 0)
(check (bytevector-advance-utf8 #u8(97) 0) => 1)
(check (bytevector-advance-utf8 #u8(97 98 99) 0) => 1)
(check (bytevector-advance-utf8 #u8(97 98 99) 1) => 2)
(check (bytevector-advance-utf8 #u8(97 98 99) 2) => 3)
(check (bytevector-advance-utf8 #u8(97 98 99) 3) => 3)
(check (bytevector-advance-utf8 #u8(228 184 150 231 149 140) 0) => 3)
(check (bytevector-advance-utf8 #u8(228 184 150 231 149 140) 3) => 6)
(check (bytevector-advance-utf8 #u8(240 159 152 128) 0) => 4)
(check (bytevector-advance-utf8 #u8(228 184) 0) => 0)
(check (bytevector-advance-utf8 #u8(255) 0) => 0)
(check (bytevector-advance-utf8 #u8(97 98) 0 1) => 1)
(check (bytevector-advance-utf8 #u8(97 98) 1 1) => 1)
;; 2 字节序列
(check (bytevector-advance-utf8 #u8(195 164 72) 0) => 2)
(check (bytevector-advance-utf8 #u8(195 169 101) 0) => 2)
;; 3 字节序列
(check (bytevector-advance-utf8 #u8(228 184 173 72) 0) => 3)
(check (bytevector-advance-utf8 #u8(230 177 137 101) 0) => 3)
;; 4 字节序列
(check (bytevector-advance-utf8 #u8(240 159 145 141 72) 0) => 4)
(check (bytevector-advance-utf8 #u8(240 159 154 128 101) 0) => 4)
;; 多字节混合序列的逐字符前进
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141) 0)
  =>
  1
) ;check
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141) 1)
  =>
  3
) ;check
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141) 3)
  =>
  6
) ;check
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141) 6)
  =>
  10
) ;check
;; 无效与截断序列停留在当前索引
(check (bytevector-advance-utf8 #u8(128) 0) => 0)
(check (bytevector-advance-utf8 #u8(194) 0) => 0)
(check (bytevector-advance-utf8 #u8(240 159 145) 0) => 0)
(check (bytevector-advance-utf8 #u8(194 0) 0) => 0)
(check (bytevector-advance-utf8 #u8(228 0 173) 0) => 0)
(check (bytevector-advance-utf8 #u8(240 159 0 141) 0) => 0)
;; end 参数截断多字节序列
(check (bytevector-advance-utf8 #u8(72 101 108 108 111) 0 2) => 1)
(check (bytevector-advance-utf8 #u8(195 164 72) 0 2) => 2)
(check (bytevector-advance-utf8 #u8(195 164 72) 0 3) => 2)
;; 超出序列末尾的索引
(check (bytevector-advance-utf8 #u8(72) 1) => 1)

(check-report)
