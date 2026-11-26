;; adler32
;; 计算数据的 Adler-32 校验和（与 Python zlib.adler32 语义一致）。
;;
;; 语法
;; ----
;; (adler32 data)
;; (adler32 data start)
;;
;; 参数
;; ----
;; data : string? 或 bytevector?
;; 待校验的数据，按原始字节计算（string 不做编码转换）。
;; start : exact-nonnegative-integer?
;; 续算用的起始值，默认 1（对齐 Python zlib.adler32 的 value 默认 1）。
;;
;; 返回值
;; -----
;; exact-nonnegative-integer?
;; 32 位无符号 Adler-32 值（0..#xFFFFFFFF）。
;;
;; 说明
;; ----
;; 1. 空数据返回 1（Adler-32 初值，对齐 Python）。
;; 2. "123456789" 的 Adler-32 为 #x091E01DE。
;; 3. 提供非负整数 start 可将多次计算拼接续算，等价于一次性校验全部数据。
;; 4. start 为负或非整数时抛 value-error；data 类型不符抛 type-error。

(import (liii check) (liii zlib) (liii error) (scheme base))

(check-set-mode! 'report-failed)

;; 标准校验样本
(check (adler32 "123456789") => 152961502)
(check (adler32 (bytevector 49 50 51 52 53 54 55 56 57)) => 152961502)

;; 空输入
(check (adler32 "") => 1)
(check (adler32 #u8()) => 1)

;; start 默认 1，显式传 1 等价
(check (adler32 "" 1) => 1)

;; 分块续算等价于一次性校验
(check (adler32 "abcdef" 1) => (adler32 "def" (adler32 "abc" 1)))
(check (adler32 (string-append "abc" "def") 1)
  =>
  (adler32 "def" (adler32 "abc" 1))
) ;check

;; 非法参数
(check-catch 'type-error (adler32 #\a))
(check-catch 'type-error (adler32 123))
(check-catch 'value-error (adler32 "x" -1))
(check-catch 'value-error (adler32 "x" 1.5))

(check-report)
