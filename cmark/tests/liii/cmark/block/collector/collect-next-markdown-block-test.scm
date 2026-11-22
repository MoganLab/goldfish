;; collect-next-markdown-block
;; 从行列表开头收集下一个 markdown-block。
;;
;; 语法
;; ----
;; (collect-next-markdown-block lines)
;;
;; 参数
;; ----
;; lines : list
;; 行列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 (markdown-block . remaining)。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(let ((res (collect-next-markdown-block '("# Hi" "body"))))
  (check (markdown-block-type (car res)) => 'atx-heading)
  (check (cdr res) => '("body"))
) ;let

(check-report)
