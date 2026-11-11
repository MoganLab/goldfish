;; truncate
;; 返回在靠近零的方向上最靠近给定数的整数。
;;
;;
;; 语法
;; ----
;; (truncate num )
;;
;; 参数
;; ----
;; num : real?
;; 实数
;;
;; 返回值
;; ----
;; 返回在靠近零的方向上最靠近给定数的整数，即正数向下取整，负数向上取整
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 说明
;; ----
;; 与 S7 原生函数 `s7-truncate` 的区别：
;; - `truncate` 遵循 R7RS：参数不精确时结果也不精确（(truncate 1.1) => 1.0）；
;;   `s7-truncate` 总是返回精确整数（(s7-truncate 1.1) => 1）。
;; - `truncate` 保留 -0.0 的符号，`s7-truncate` 把 -0.0 归一化为 0。
;; - `truncate` 对非实数参数抛出 type-error，`s7-truncate` 抛出 S7 原生错误。
;;
;; 选择建议：常规代码用 `truncate`（R7RS 语义、错误类型统一、可移植）；
;; 需要确定得到精确整数（例如把浮点输入规整成整数索引）或追求 S7 原生性能时用 `s7-truncate`。
;;
;; 参见: gf doc liii/s7 "s7-truncate"
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

(check (truncate 1.1) => 1.0)
(check (truncate 1) => 1)
(check (truncate 1/2) => 0)
(check (truncate 0) => 0)
(check (truncate -1) => -1)
(check (truncate -1.2) => -1.0)
(check-catch 'type-error (truncate 2.0+4.0i))
(check-catch 'type-error (truncate 'hello'))
(check-catch 'wrong-number-of-args (truncate 4 5))

(check-report)
