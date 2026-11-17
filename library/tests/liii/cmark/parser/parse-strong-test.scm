
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-strong
;; 解析 ** 或 __ 包裹的强强调内容。
;;
;; 语法
;; ----
;; (parse-strong str start)
;;
;; 描述
;; ----
;; strong-start? 用于判断当前位置是否为强强调入口。

(check-true (strong-start? "**strong**" 0))
(check-true (strong-start? "__strong__" 0))
(check-false (strong-start? "*emph*" 0))
(check-false (strong-start? "_emph_" 0))

(let ((result (parse-strong "**hello**" 0)))
  (check (cmark-node-type (car result)) => 'strong)
  (check (cdr result) => 9)
) ;let

(let ((result (parse-strong "__hello__" 0)))
  (check (cmark-node-type (car result)) => 'strong)
  (check (cdr result) => 9)
) ;let

;; 未闭合时只消费起始标记的一个字符，剩余 run 由调度继续解析（最终合并为文本；Example 442 拆 run 需要）
(let ((result (parse-strong "**unclosed" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

;; 未闭合且后续有换行：不能吞掉换行（CommonMark Example 46）
(let ((result (parse-strong "**\n__" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

(check-report)
