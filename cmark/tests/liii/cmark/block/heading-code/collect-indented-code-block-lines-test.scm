;; collect-indented-code-block-lines
;; 收集缩进代码块内容及剩余行。
;;
;; 语法
;; ----
;; (collect-indented-code-block-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 行列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 (code-lines . remaining)。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(let ((res (collect-indented-code-block-lines '("    a" "    b" "after"))))
  (check (car res) => '("a" "b"))
  (check (cdr res) => '("after"))
) ;let

(check-report)
