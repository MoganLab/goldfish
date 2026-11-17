;; parse-linebreak
;; 解析文本中的软换行与硬换行。
;;
;; 语法
;; ----
;; (parse-linebreak str start)
;;
;;
;; 参数
;; ----
;; str : any
;; 输入参数。
;; start : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; linebreak? 用于判断当前位置是否是换行字符。

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (linebreak? "line1\nline2" 5))
(check-true (linebreak? "line1\\\nline2" 5))
(check-false (linebreak? "line1 line2" 5))

(let ((result (parse-linebreak "line1\n" 5)))
  (check (cmark-node-type (car result)) => 'softbreak)
  (check (cdr result) => 6)
) ;let

(let ((result (parse-linebreak "line1  \n" 7)))
  (check (cmark-node-type (car result)) => 'linebreak)
  (check (cdr result) => 8)
) ;let

(let ((result (parse-linebreak "line1\\\n" 5)))
  (check (cmark-node-type (car result)) => 'linebreak)
  (check (cdr result) => 7)
) ;let

(check-report)
