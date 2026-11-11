;; s7-truncate
;; S7 内置的向零截断函数，总是返回精确整数。
;;
;;
;; 语法
;; ----
;; (s7-truncate x)
;;
;; 参数
;; ----
;; x : real?
;; 待截断的实数。
;;
;; 返回值
;; ----
;; real?
;; 向零方向截断后的整数。
;;
;; 说明
;; ----
;; 1. 与 `truncate` 的区别：`s7-truncate` 总是返回精确整数（(s7-truncate 1.1) => 1），
;;    而 `truncate` 在参数不精确时返回不精确整数（(truncate 1.1) => 1.0）；
;;    `s7-truncate` 把 -0.0 归一化为 0，`truncate` 保留 -0.0 的符号。
;; 2. `s7-truncate` 不做 R7RS 的参数检查，非实数参数会抛出 S7 原生错误。
;; 3. 对于整数返回自身。
;;
;; 选择建议：常规代码用 `truncate`；需要确定得到精确整数（例如把浮点输入规整成整数索引）
;; 或追求 S7 原生性能时用 `s7-truncate`。
;;
;; 参见: gf doc scheme/base "truncate"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-truncate 3.7) => 3)
(check (s7-truncate -3.7) => -3)
(check (s7-truncate 5) => 5)
(check (s7-truncate 2.0) => 2)

(check-report)
