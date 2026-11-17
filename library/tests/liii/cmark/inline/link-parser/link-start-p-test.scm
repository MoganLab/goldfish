;; link-start?
;; 判断位置 pos 是否为 [ 链接起始。
;;
;; 语法
;; ----
;; (link-start? str pos)
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
;; 是 [ 返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link-parser))
(import (liii cmark node))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(check-true (link-start? "[a](b)" 0))
(check-false (link-start? "a" 0))

(check-report)
