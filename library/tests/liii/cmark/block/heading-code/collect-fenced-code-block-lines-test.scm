;; collect-fenced-code-block-lines
;; 收集围栏代码块行及剩余行。
;;
;; 语法
;; ----
;; (collect-fenced-code-block-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 行列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 (collected . remaining)。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(let ((res (collect-fenced-code-block-lines '("```" "code" "```" "after"))))
  (check (cdr res) => '("after"))
) ;let

(check-report)
