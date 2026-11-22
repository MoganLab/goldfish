;; collect-blockquote-lines-with-lazy
;; 收集含惰性续行的引用块行。
;;
;; 语法
;; ----
;; (collect-blockquote-lines-with-lazy lines)
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

(let ((res (collect-blockquote-lines-with-lazy '("> a" "lazy" "" "after"))))
  (check (car res) => '("> a" "lazy"))
  (check (cdr res) => '("" "after"))
) ;let

(check-report)
