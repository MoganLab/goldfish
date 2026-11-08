;; nothing
;; 不产生任何输出的占位组合子。
;;
;; 语法
;; ----
;; nothing
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; format?
;; 求值后不输出任何字符、也不改变行列的组合子。
;;
;; 说明
;; ----
;; 常用作条件分支的空分支，例如
;; `(if first? nothing (each ","))`。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f "a" nothing "b") => "ab")
(check (show #f nothing) => "")
(check (show #f "ab" nothing (fn (col) (each "[" col "]"))) => "ab[2]")

(check-report)
