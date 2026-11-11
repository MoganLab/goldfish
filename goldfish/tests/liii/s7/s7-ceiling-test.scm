;; s7-ceiling
;; S7 内置的向上取整函数，总是返回精确整数。
;;
;;
;; 语法
;; ----
;; (s7-ceiling x)
;;
;; 参数
;; ----
;; x : real?
;; 待取整的实数。
;;
;; 返回值
;; ----
;; real?
;; 不小于 x 的最小整数。
;;
;; 说明
;; ----
;; 1. 与 `ceiling` 的区别：`s7-ceiling` 总是返回精确整数（(s7-ceiling 1.1) => 2），
;;    而 `ceiling` 在参数不精确时返回不精确整数（(ceiling 1.1) => 2.0）；
;;    `s7-ceiling` 把 -0.0 归一化为 0，`ceiling` 保留 -0.0 的符号。
;; 2. `s7-ceiling` 不做 R7RS 的参数检查，非实数参数会抛出 S7 原生错误。
;; 3. 对于整数返回自身。
;;
;; 选择建议：常规代码用 `ceiling`；需要确定得到精确整数（例如把浮点输入规整成整数索引）
;; 或追求 S7 原生性能时用 `s7-ceiling`。
;;
;; 参见: gf doc scheme/base "ceiling"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-ceiling 3.2) => 4)
(check (s7-ceiling -3.2) => -3)
(check (s7-ceiling 5) => 5)
(check (s7-ceiling 2.0) => 2)

(check-report)
