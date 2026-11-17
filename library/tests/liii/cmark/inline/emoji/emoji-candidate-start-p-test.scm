;; emoji-candidate-start?
;; 判断位置 pos 是否为 Emoji 短代码候选起始。
;;
;; 语法
;; ----
;; (emoji-candidate-start? str pos)
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
;; 是候选起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emoji))

(check-set-mode! 'report-failed)

(check-true (emoji-candidate-start? ":smile:" 0))
(check-false (emoji-candidate-start? "smile" 0))

(check-report)
