;; strong-start?
;; 判断位置 pos 是否可能是 ** 或 __ 粗体起始。
;;
;; 语法
;; ----
;; (strong-start? str pos)
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
;; 是粗体起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emphasis))

(check-set-mode! 'report-failed)

(check-true (strong-start? "**a**" 0))
(check-false (strong-start? "*a*" 0))

(check-report)
