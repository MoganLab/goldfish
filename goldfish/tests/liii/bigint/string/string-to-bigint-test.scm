;; string->bigint
;; 将指定进制的字符串解析为 bigint。
;;
;; 语法
;; ----
;; (string->bigint str)
;; (string->bigint str radix)
;;
;; 参数
;; ----
;; str : string?
;;   指定进制下的字符串，可带前导正负号。
;;
;; 返回值
;; ----
;; bigint?
;;   解析得到的 bigint。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数不是字符串时抛出 type-error；空串、非法字符或非法 radix 时抛出 value-error。

(import (liii check)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(define hundred-digits
  "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
) ;define

(check (bigint->string (string->bigint "0")) => "0")
(check (bigint->string (string->bigint "-42")) => "-42")
(check (bigint->string (string->bigint "+007")) => "7")
(check (bigint->string (string->bigint "0000000000")) => "0")
(check (bigint->string (string->bigint hundred-digits)) => hundred-digits)
(check (bigint->string (string->bigint "ff" 16)) => "255")
(check (bigint->string (string->bigint "1010" 2)) => "10")
(check (bigint->string (string->bigint "z" 36)) => "35")

(check-catch 'value-error (string->bigint ""))
(check-catch 'value-error (string->bigint "+"))
(check-catch 'value-error (string->bigint "-"))
(check-catch 'value-error (string->bigint "12x"))
(check-catch 'type-error (string->bigint 123))
(check-catch 'value-error (string->bigint "2" 2))
(check-catch 'value-error (string->bigint "10" 1))

(check-report)
