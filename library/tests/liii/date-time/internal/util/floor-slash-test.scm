;; floor/
;; 对两个整数做向下取整除法，返回商与余数两个值。
;;
;; 语法
;; ----
;; (floor/ n d)
;;
;; 参数
;; ----
;; n : integer? 被除数。
;; d : integer? 除数。
;;
;; 返回值
;; ----
;; values 两个 integer?：向下取整的商与同号非负的余数。

(import (liii check) (liii date-time internal util))

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (floor/ 7 2)) list) => '(3 1))
(check (call-with-values (lambda () (floor/ -7 2)) list) => '(-4 1))
(check (call-with-values (lambda () (floor/ 8 4)) list) => '(2 0))

(check-report)
