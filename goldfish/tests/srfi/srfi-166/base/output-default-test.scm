;; output-default
;; 默认输出过程：写出字符串并推进行列状态。
;;
;; 语法
;; ----
;; (output-default str)
;;
;; 参数
;; ----
;; str : string?
;; 要写出的字符串。
;;
;; 返回值
;; ----
;; format?
;; 写出 str 并更新 row、col 的组合子。
;;
;; 说明
;; ----
;; 字符串含换行时，行号按换行次数递增、列号取最后一行长度；
;; 它是状态变量 output 的默认值，替换它即可改变全部输出的
;; 落地方式。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (output-default "ab") (fn (col) (each "[" col "]"))) => "ab[2]")
(check (show #f (output-default "a\nbc") (fn (row col) (each "[" row "," col "]")))
  =>
  "a\nbc[1,2]"
) ;check
(check (show #f (output-default "")) => "")

(check-report)
