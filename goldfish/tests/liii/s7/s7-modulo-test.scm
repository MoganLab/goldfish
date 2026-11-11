;; s7-modulo
;; S7 内置的模运算函数。
;;
;; 语法
;; ----
;; (s7-modulo dividend divisor)
;;
;; 参数
;; ----
;; dividend : integer?
;; 被除数。
;; divisor : integer?
;; 除数，不能为零。
;;
;; 返回值
;; ----
;; integer?
;; 返回 dividend 除以 divisor 的模。
;;
;; 说明
;; ----
;; 结果的符号与除数 divisor 相同。
;;
;; 参见: gf doc scheme/base "modulo"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-modulo 13 4) => 1)
(check (s7-modulo -13 4) => 3)
(check (s7-modulo 13 -4) => -3)
(check (s7-modulo -13 -4) => -1)

(check-report)
