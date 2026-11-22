;; setext-heading-start?
;; 判断两行及以上是否构成 Setext 标题。
;;
;; 语法
;; ----
;; (setext-heading-start? lines)
;;
;; 参数
;; ----
;; lines : list
;; 行列表。
;;
;; 返回值
;; ----
;; boolean
;; 构成 Setext 标题返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block heading-code))

(check-set-mode! 'report-failed)

(check-true (setext-heading-start? '("Title" "===")))
(check-false (setext-heading-start? '("" "===")))

(check-report)
