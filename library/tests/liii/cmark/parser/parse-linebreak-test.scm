
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-linebreak
;; 解析文本中的软换行与硬换行。
;;
;; 语法
;; ----
;; (parse-linebreak str start)
;;
;; 描述
;; ----
;; linebreak? 用于判断当前位置是否是换行字符。

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
