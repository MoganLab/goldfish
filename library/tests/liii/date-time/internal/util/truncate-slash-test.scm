;; truncate/
;; 对两个整数做截断除法，返回商与余数两个值。
;;
;; 语法
;; ----
;; (truncate/ n d)
;;
;; 参数
;; ----
;; n : integer? 被除数。
;; d : integer? 除数。
;;
;; 返回值
;; ----
;; values 两个 integer?：向零截断的商与余数。

(import (liii check) (liii date-time internal util))

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (truncate/ 7 2)) list) => '(3 1))
(check (call-with-values (lambda () (truncate/ -7 2)) list) => '(-3 -1))
(check (call-with-values (lambda () (truncate/ 8 4)) list) => '(2 0))

(check-report)
