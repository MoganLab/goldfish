;; block-start?
;; 判断行列表首行是否启动了非段落块结构。
;;
;; 语法
;; ----
;; (block-start? lines)
;;
;; 参数
;; ----
;; lines : list
;; 行列表。
;;
;; 返回值
;; ----
;; boolean
;; 启动块结构返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(check-true (block-start? '("# heading")))
(check-false (block-start? '("plain text")))

(check-report)
