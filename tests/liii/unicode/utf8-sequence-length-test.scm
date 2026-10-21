(import (liii check) (liii unicode))

(check-set-mode! 'report-failed)

;; utf8-sequence-length
;; 由 UTF-8 前导字节推断该字符序列的字节数。
;;
;; 语法
;; ----
;; (utf8-sequence-length lead-byte)
;;
;; 参数
;; ----
;; lead-byte : integer
;; UTF-8 序列的首字节（0-255）。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 合法前导字节返回该字符占用的字节数（1/2/3/4）；
;; 连续字节（0x80-0xBF）与 0xF5-0xFF 等非法前导字节返回 #f。
;;
;; 描述
;; ----
;; UTF-8 字符的字节数由首字节的高位决定：
;; - 0xxxxxxx（0-127）：1 字节；
;; - 110xxxxx（194-223）：2 字节；
;; - 1110xxxx（224-239）：3 字节；
;; - 11110xxx（240-244）：4 字节。
;;
;; 0x80-0xBF 是连续字节，不能作为序列首字节；0xF5-0xFF 超出 Unicode 编码范围。
;; 读取多字节字符时，可先用本函数确定还需读取多少个连续字节。
;;
;; 相关函数
;; ----
;; (liii unicode) utf8->codepoint — 由完整字节序列解码出码点。
;; (liii unicode) utf8-substring — 按 Unicode 字符位置提取子字符串。

;; 单字节字符
(check (utf8-sequence-length 0) => 1)
(check (utf8-sequence-length 65) => 1)
(check (utf8-sequence-length 127) => 1)

;; 双字节字符（0xC2-0xDF）
(check (utf8-sequence-length 194) => 2)
(check (utf8-sequence-length 223) => 2)

;; 三字节字符（0xE0-0xEF）
(check (utf8-sequence-length 224) => 3)
(check (utf8-sequence-length 239) => 3)

;; 四字节字符（0xF0-0xF4）
(check (utf8-sequence-length 240) => 4)
(check (utf8-sequence-length 244) => 4)

;; 连续字节不能作为首字节
(check (utf8-sequence-length 128) => #f)
(check (utf8-sequence-length 191) => #f)

;; 超出 Unicode 编码范围的首字节
(check (utf8-sequence-length 245) => #f)
(check (utf8-sequence-length 255) => #f)

;; 与 utf8->codepoint 的实际序列长度一致
(check (utf8-sequence-length (bytevector-u8-ref (string->utf8 "a") 0)) => 1)
(check (utf8-sequence-length (bytevector-u8-ref (string->utf8 "é") 0)) => 2)
(check (utf8-sequence-length (bytevector-u8-ref (string->utf8 "你") 0)) => 3)
(check (utf8-sequence-length (bytevector-u8-ref (string->utf8 "🚀") 0)) => 4)

(check-report)
