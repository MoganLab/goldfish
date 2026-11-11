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
;; 1. 与 `modulo` 的区别：`modulo` 先校验参数必须是实数，并对除数为 0 抛出 division-by-zero；
;;    `s7-modulo` 交由 S7 内建处理，除数为 0 时不报错（(s7-modulo 13 0) => 13）。
;; 2. 两者都允许实数参数（(s7-modulo 13.5 4) => 1.5）。
;; 3. `s7-modulo` 不做 R7RS 的参数检查，非法参数会抛出 S7 原生错误。
;;
;; 选择建议：常规代码用 `modulo`；确认参数合法且只追求 S7 原生性能时用 `s7-modulo`。
;;
;; 参见: gf doc scheme/base "modulo"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-modulo 13 4) => 1)
(check (s7-modulo -13 4) => 3)
(check (s7-modulo 13 -4) => -3)
(check (s7-modulo -13 -4) => -1)

(check-report)
