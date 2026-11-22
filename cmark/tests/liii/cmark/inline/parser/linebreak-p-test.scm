;; linebreak?
;; 判断位置 pos 是否为换行符位置。
;;
;; 语法
;; ----
;; (linebreak? str pos)
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
;; 是换行位置返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(check-true (linebreak? "a\nb" 1))
(check-false (linebreak? "ab" 0))

(check-report)
