;; ceiling
;; 返回不小于给定数的最小整数。
;;
;;
;; 语法
;; ----
;; (ceiling num )
;;
;; 参数
;; ----
;; num : real?
;; 实数
;;
;; 返回值
;; ----
;; 返回不小于给定数的最小整数
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 说明
;; ----
;; 与 S7 原生函数 `s7-ceiling` 的区别：
;; - `ceiling` 遵循 R7RS：参数不精确时结果也不精确（(ceiling 1.1) => 2.0）；
;;   `s7-ceiling` 总是返回精确整数（(s7-ceiling 1.1) => 2）。
;; - `ceiling` 保留 -0.0 的符号，`s7-ceiling` 把 -0.0 归一化为 0。
;; - `ceiling` 对非实数参数抛出 type-error，`s7-ceiling` 抛出 S7 原生错误。
;;
;; 选择建议：常规代码用 `ceiling`（R7RS 语义、错误类型统一、可移植）；
;; 需要确定得到精确整数（例如把浮点输入规整成整数索引）或追求 S7 原生性能时用 `s7-ceiling`。
;;
;; 参见: gf doc liii/s7 "s7-ceiling"
;;
;; 错误处理
;; ----
;; type-error
;; 如果参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果参数数量不为一，抛出错误。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (ceiling 1.1) => 2.0)
(check (ceiling 1) => 1)
(check (ceiling 1/2) => 1)
(check (ceiling 0) => 0)
(check (ceiling -1) => -1)
(check (ceiling -1.2) => -1.0)
(check-catch 'type-error (ceiling 2.0+4.0i))
(check-catch 'type-error (ceiling 'hello'))
(check-catch 'wrong-number-of-args (ceiling 4 5))

(check-report)
