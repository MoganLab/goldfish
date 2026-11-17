;; collect-alert-block-lines
;; 从行列表中收集连续的 Alert 块内容及剩余行。
;;
;; 语法
;; ----
;; (collect-alert-block-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 ((kind . body-lines) . remaining-lines)。

(import (scheme base))
(import (liii check))
(import (liii cmark block alert))

(check-set-mode! 'report-failed)

(let ((res (collect-alert-block-lines '("> [!NOTE]" "> body" "after"))))
  (check (caar res) => '(kind . note))
  (check (cdar res) => '((lines "body")))
  (check (cdr res) => '("after"))
) ;let

(check-report)
