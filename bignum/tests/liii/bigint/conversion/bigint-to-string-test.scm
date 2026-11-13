;; bigint->string
;; 将 bigint 转换为指定进制的字符串。
;;
;; 语法
;; ----
;; (bigint->string n)
;; (bigint->string n radix)
;;
;; 参数
;; ----
;; n : bigint?
;;   要输出的 bigint。
;;
;; 返回值
;; ----
;; string?
;;   指定进制下的字符串表示。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数不是 bigint 时抛出 type-error；非法 radix 时抛出 value-error。

(import (liii check)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (string->bigint "0")) => "0")
(check (bigint->string (string->bigint "1")) => "1")
(check (bigint->string (string->bigint "-999999999999999999")) => "-999999999999999999")
(check (bigint->string (string->bigint "4294967296")) => "4294967296")
(check (bigint->string (string->bigint "255") 16) => "ff")
(check (bigint->string (string->bigint "255") 2) => "11111111")
(check (bigint->string (string->bigint "-35") 36) => "-z")

(check-catch 'type-error (bigint->string 123))
(check-catch 'type-error (bigint->string "123"))
(check-catch 'value-error (bigint->string (string->bigint "10") 37))

(check-report)
