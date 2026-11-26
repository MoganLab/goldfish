;; crc32
;; 计算数据的 CRC-32 校验和（与 Python zlib.crc32 语义一致）。
;;
;; 语法
;; ----
;; (crc32 data)
;; (crc32 data start)
;;
;; 参数
;; ----
;; data : string? 或 bytevector?
;; 待校验的数据，按原始字节计算（string 不做编码转换）。
;; start : exact-nonnegative-integer?
;; 续算用的起始值，默认 0（对齐 Python zlib.crc32 的 value 默认 0）。
;;
;; 返回值
;; -----
;; exact-nonnegative-integer?
;; 32 位无符号 CRC-32 值（0..#xFFFFFFFF）。
;;
;; 说明
;; ----
;; 1. 空数据返回 0（对齐 Python）。
;; 2. "123456789" 的 CRC-32 为 #xCBF43926（CRC-32 标准校验样本）。
;; 3. 提供非负整数 start 可将多次计算拼接续算，等价于一次性校验全部数据。
;; 4. start 为负或非整数时抛 value-error；data 类型不符抛 type-error。

(import (liii check) (liii zlib) (liii error) (scheme base))

(check-set-mode! 'report-failed)

;; 标准校验样本
(check (crc32 "123456789") => 3421780262)
(check (crc32 (bytevector 49 50 51 52 53 54 55 56 57)) => 3421780262)

;; 空输入
(check (crc32 "") => 0)
(check (crc32 #u8()) => 0)

;; start 默认 0，显式传 0 等价
(check (crc32 "" 0) => 0)

;; 分块续算等价于一次性校验
(check (crc32 (string-append "abc" "def") 0)
  =>
  (crc32 (string-append "def" "") (crc32 "abc" 0))
) ;check
(check (crc32 "abcdef" 0) => (crc32 "def" (crc32 "abc" 0)))

;; 非法参数
(check-catch 'type-error (crc32 #\a))
(check-catch 'type-error (crc32 123))
(check-catch 'value-error (crc32 "x" -1))
(check-catch 'value-error (crc32 "x" 1.5))

(check-report)
