;; strikeout-start?
;; 判断位置 pos 是否为删除线 ~~ 起始。
;;
;; 语法
;; ----
;; (strikeout-start? str pos)
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
;; 是删除线起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline util))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (strikeout-start? "~~x~~" 0))
(check-false (strikeout-start? "~~~x~~~" 0))

(check-report)
