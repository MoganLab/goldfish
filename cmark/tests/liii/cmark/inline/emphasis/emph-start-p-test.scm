;; emph-start?
;; 判断位置 pos 是否可能是单个 * 或 _ 强调起始。
;;
;; 语法
;; ----
;; (emph-start? str pos)
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
;; 是强调起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emphasis))

(check-set-mode! 'report-failed)

(check-true (emph-start? "*a*" 0))
(check-false (emph-start? "* a*" 0))

(check-report)
