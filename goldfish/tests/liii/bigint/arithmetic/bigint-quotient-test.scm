;; bigint-quotient
;; bigint-divide 的别名。
;;
;; 语法
;; ----
;; (bigint-quotient a b)
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

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-quotient (b "100") (b "3"))) => "33")
(check (bigint->string (bigint-quotient (b "-100") (b "3"))) => "-33")

(check-report)
