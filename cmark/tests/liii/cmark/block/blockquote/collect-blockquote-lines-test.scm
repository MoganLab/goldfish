;; collect-blockquote-lines
;; 收集连续的引用行并返回 (collected . remaining)。
;;
;; 语法
;; ----
;; (collect-blockquote-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 (collected-lines . remaining-lines)。

(import (scheme base))
(import (liii check))
(import (liii cmark block blockquote))

(check-set-mode! 'report-failed)

(check (collect-blockquote-lines '("> a" "> b" "c")) => '(("> a" "> b") "c"))

(check-report)
