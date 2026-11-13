;; bigint-divide
;; 计算两个 bigint 的整数商，按 0 方向截断。
;;
;; 语法
;; ----
;; (bigint-divide a b)
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
;;   截断除法的商。
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

(check (bigint->string (bigint-divide (b "100") (b "3"))) => "33")
(check (bigint->string (bigint-divide (b "-100") (b "3"))) => "-33")
(check (bigint->string (bigint-divide (b "100") (b "-3"))) => "-33")
(check (bigint->string (bigint-divide (b "-100") (b "-3"))) => "33")
(check (bigint->string (bigint-divide (b "2") (b "3"))) => "0")

(check-catch 'value-error (bigint-divide (b "1") bigint-zero))
(check-catch 'type-error (bigint-divide 1 (b "2")))

(check-report)
