;; collect-math-block-lines
;; 从行列表中收集块级数学公式内容及剩余行。
;;
;; 语法
;; ----
;; (collect-math-block-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 (content-string . remaining-lines)。

(import (scheme base))
(import (liii check))
(import (liii cmark block math))

(check-set-mode! 'report-failed)

(let ((res (collect-math-block-lines '("$$" "x+y" "$$" "after"))))
  (check (car res) => '("$$" "x+y" "$$"))
  (check (cdr res) => '("after"))
) ;let

(check-report)
