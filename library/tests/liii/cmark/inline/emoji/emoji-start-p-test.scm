;; emoji-start?
;; 判断位置 pos 是否为受支持的 Emoji 短代码起始。
;;
;; 语法
;; ----
;; (emoji-start? str pos)
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
;; 受支持返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emoji))

(check-set-mode! 'report-failed)

(check-true (emoji-start? ":smile:" 0))
(check-false (emoji-start? ":unknown:" 0))

(check-report)
