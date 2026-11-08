;; tabular
;; 表格组合子：把多列等宽内容按行对齐成表格。
;;
;; 语法
;; ----
;; (tabular [width | 'infinite] col ...)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 可选，列宽；'infinite 表示该列不占宽（常用于分隔列）。
;;
;; col : format?
;; 各列内容，列之间的分隔串同样作为参数传入。
;;
;; 返回值
;; ----
;; format?
;; 输出整张表格的格式化组合子。
;;
;; 说明
;; ----
;; 列宽默认取该列最宽一行的宽度，因此多行内容会自然对齐；
;; 底层由 columnar 完成排版。
;; `(liii format columnar)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

(check
  (show #f
    (with ((width 20)) (tabular (each "a\nbc\ndef\n") " | " (each "123\n45\n6\n")))
  ) ;show
  =>
  "a   | 123\nbc  | 45\ndef | 6\n"
) ;check
(check (show #f (tabular)) => "\n")

(check-report)
