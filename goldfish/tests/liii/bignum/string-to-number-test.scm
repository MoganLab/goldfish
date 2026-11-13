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

(check-report)
