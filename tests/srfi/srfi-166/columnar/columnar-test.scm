(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

;; columnar
;; 多列排版组合子。支持多列并排输出、列宽设置、自动伸缩和对齐（left, right, center）。
;;
;; 语法
;; ----
;; (columnar ['infinite|'right|'left|'center|width] col ...)
;; (tabular col ...)
;; (line-numbers [start])
;;
;; 示例
;; ----
;; (show #f (columnar (each "*"))) => "*\n"
;;
;; 另见
;; ----
;; (liii format columnar) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format columnar))
;;   (show #f (columnar (each "*")))

(check (show #f (columnar)) => "\n")
(check (show #f (columnar (each "*"))) => "*\n")

(check (show #f (with ((width 20))
                  (tabular (each "a\nbc\ndef\n") " | "
                           (each "123\n45\n6\n"))))
       => "a   | 123\nbc  | 45\ndef | 6\n")

(check-report)
