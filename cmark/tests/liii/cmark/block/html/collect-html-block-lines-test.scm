;; collect-html-block-lines
;; 从行列表中收集块级 HTML 内容及剩余行。
;;
;; 语法
;; ----
;; (collect-html-block-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 (html-lines . remaining-lines)。

(import (scheme base))
(import (liii check))
(import (liii cmark block html))

(check-set-mode! 'report-failed)

(let ((res (collect-html-block-lines '("<div>" "hi" "</div>" "" "after"))))
  (check (car res) => '("<div>" "hi" "</div>"))
  (check (cdr res) => '("" "after"))
) ;let

(check-report)
