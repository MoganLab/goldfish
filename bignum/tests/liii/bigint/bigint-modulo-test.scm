;; bigint-modulo
;; 计算整数除法的模（R7RS 语义：非零结果与除数同号）。
;;
;; 语法
;; ----
;; (bigint-modulo a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   被模数。
;; b : bigint?
;;   模数。
;;
;; 返回值
;; ----
;; bigint?
;;   与除数 b 同号的模结果。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   当参数不是 bigint 时抛出 type-error；模数为零时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-modulo (b "100") (b "3"))) => "1")
(check (bigint->string (bigint-modulo (b "-100") (b "3"))) => "2")
(check (bigint->string (bigint-modulo (b "100") (b "-3"))) => "-2")
(check (bigint->string (bigint-modulo (b "-100") (b "-3"))) => "-1")

(check-catch 'value-error (bigint-modulo (b "1") bigint-zero))

(check-report)
