;; columnar
;; 多列并排排版组合子：按列宽与对齐方式横向排列内容。
;;
;; 语法
;; ----
;; (columnar [width | 'infinite | 'right | 'left | 'center] col ...)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 可选，该列的显示宽度；也可以用 'infinite 表示不占宽、
;; 'right / 'left / 'center 指定该列对齐方式。
;;
;; col : format?
;; 该列的格式化对象，列之间的分隔串同样作为参数传入。
;;
;; 返回值
;; ----
;; format?
;; 把各列内容并排输出并补一个换行的格式化组合子。
;;
;; 说明
;; ----
;; 内容按行横向拼接，短列用空白补齐；tabular 是它的
;; 常用封装。`(liii format columnar)` 以 Goldfish 命名
;; 重导出本库全部函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

(check (show #f (columnar)) => "\n")
(check (show #f (columnar (each "*"))) => "*\n")
(check (show #f (columnar 4 'right (each "ab") " " (each "cd"))) => "  ab cd\n")
(check (show #f (columnar 4 'center (each "ab") "|" (each "cdef")))
  =>
  " ab |cdef\n"
) ;check

(check-report)
