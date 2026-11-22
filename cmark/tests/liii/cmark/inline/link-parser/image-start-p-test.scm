;; image-start?
;; 判断位置 pos 是否为 ![ 图片起始。
;;
;; 语法
;; ----
;; (image-start? str pos)
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
;; 是 ![ 返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link-parser))
(import (liii cmark node))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(check-true (image-start? "![a](b)" 0))
(check-false (image-start? "[a](b)" 0))

(check-report)
