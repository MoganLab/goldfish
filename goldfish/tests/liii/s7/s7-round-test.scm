;; s7-round
;; S7 内置的四舍五入函数（半数时向偶数舍入），总是返回精确整数。
;;
;;
;; 语法
;; ----
;; (s7-round x)
;;
;; 参数
;; ----
;; x : real?
;; 待四舍五入的实数。
;;
;; 返回值
;; ----
;; real?
;; 最接近 x 的整数，半数时向偶数舍入。
;;
;; 说明
;; ----
;; 1. 与 `round` 的区别：`s7-round` 总是返回精确整数（(s7-round 1.1) => 1），
;;    而 `round` 在参数不精确时返回不精确整数（(round 1.1) => 1.0）；
;;    `s7-round` 把 -0.5 归一化为 0，`round` 返回 -0.0。
;; 2. 两者都采用银行家舍入（半数时向偶数靠近）。
;; 3. `s7-round` 不做 R7RS 的参数检查，非实数参数会抛出 S7 原生错误。
;;
;; 选择建议：常规代码用 `round`；需要确定得到精确整数（例如把浮点输入规整成整数索引）
;; 或追求 S7 原生性能时用 `s7-round`。
;;
;; 参见: gf doc scheme/base "round"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-round 3.5) => 4)
(check (s7-round 2.5) => 2)
(check (s7-round -3.5) => -4)
(check (s7-round 3.2) => 3)
(check (s7-round 5) => 5)

(check-report)
