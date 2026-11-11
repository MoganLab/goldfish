;; s7-lcm
;; S7 内置的最小公倍数函数，只接受有理（精确）参数。
;;
;;
;; 语法
;; ----
;; (s7-lcm n1 ...)
;;
;; 参数
;; ----
;; n1 ... : integer?
;; 一个或多个整数。
;;
;; 返回值
;; ----
;; integer?
;; 参数的最小公倍数。
;;
;; 说明
;; ----
;; 1. 与 `lcm` 的区别：`s7-lcm` 只接受有理（精确）参数，传入不精确数会报错；
;;    `lcm` 接受不精确参数并返回不精确结果（(lcm 2 3.0) => 6.0）。
;; 2. 两者对空参数列表都返回 1。
;; 3. `s7-lcm` 不做 R7RS 的参数检查，非法参数会抛出 S7 原生错误。
;;
;; 选择建议：常规代码用 `lcm`；参数确定全为精确有理数且只追求 S7 原生性能时用 `s7-lcm`。
;;
;; 参见: gf doc scheme/base "lcm"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-lcm 4 6) => 12)
(check (s7-lcm 3 5 7) => 105)
(check (s7-lcm) => 1)
(check (s7-lcm 7) => 7)
(check (s7-lcm -4 6) => 12)

(check-report)
