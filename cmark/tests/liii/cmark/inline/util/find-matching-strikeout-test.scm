;; find-matching-strikeout
;; 从 start 起查找闭合的 ~~ 删除线位置。
;;
;; 语法
;; ----
;; (find-matching-strikeout str start)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; 起始位置。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 找到返回闭合位置，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline util))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (find-matching-strikeout "~~ab~~" 2) => 4)
(check (find-matching-strikeout "~~ab" 2) => #f)

(check-report)
