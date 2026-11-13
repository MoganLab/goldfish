;; bigint-remainder
;; 计算截断除法下的余数，余数符号与被除数相同。
;;
;; 语法
;; ----
;; (bigint-remainder a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   被除数。
;; b : bigint?
;;   除数。
;;
;; 返回值
;; ----
;; bigint?
;;   截断除法的余数。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   当参数不是 bigint 时抛出 type-error；除数为零时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-remainder (b "100") (b "3"))) => "1")
(check (bigint->string (bigint-remainder (b "-100") (b "3"))) => "-1")
(check (bigint->string (bigint-remainder (b "100") (b "-3"))) => "1")
(check (bigint->string (bigint-remainder (b "-100") (b "-3"))) => "-1")
(check (bigint->string (bigint-remainder (b "2") (b "3"))) => "2")

(check-catch 'value-error (bigint-remainder (b "1") bigint-zero))
(check-catch 'type-error (bigint-remainder 1 (b "2")))

(check-report)
