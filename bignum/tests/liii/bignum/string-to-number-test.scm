;; string->number
;; 将字符串解析为数值。
;;
;; 语法
;; ----
;; (string->number str [radix])
;;
;; 参数
;; ----
;; str : string? 输入字符串。
;; radix
;; :
;; integer?
;; 可选进制，默认
;; 10。
;;
;; 返回值
;; ----
;; number? | boolean? 解析成功返回数值，失败返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (string->number "42") => 42)
(check (string->number "deadbeef" 16) => 3735928559)
(check (string->number "123456789012345678901234567890")
  =>
  123456789012345678901234567890
) ;check
(check (string->number "#e1e20") => 100000000000000000000)
(check (string->number "#e1.5e2") => 150)
(check (string->number "#i123456789012345678901234567890")
  =>
  1.2345678901234568e+29
) ;check
(check (string->number "200000000000000000000/2") => 100000000000000000000)
(check (string->number "100000000000000000000/3") => 100000000000000000000/3)
(check (string->number "100000000000000000001+2i") => 100000000000000000001+2i)
(check (string->number "123" 2) => #f)

(check-report)
