;; inline-html-start?
;; 判断位置 pos 是否为行内 HTML 起始。
;;
;; 语法
;; ----
;; (inline-html-start? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 位置索引。
;;
;; 返回值
;; ----
;; boolean
;; 是合法行内 HTML 起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline html))

(check-set-mode! 'report-failed)

(check-true (inline-html-start? "<span>" 0))
(check-false (inline-html-start? "<123>" 0))

(check-report)
