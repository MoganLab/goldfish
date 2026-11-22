;; collect-paragraph-lines
;; 收集连续的段落行。
;;
;; 语法
;; ----
;; (collect-paragraph-lines lines)
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
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(let ((res (collect-paragraph-lines '("a" "b" "# h"))))
  (check (car res) => '("a" "b"))
  (check (cadr res) => '("# h"))
) ;let

(check-report)
