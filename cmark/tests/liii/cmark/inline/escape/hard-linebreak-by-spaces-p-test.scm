;; hard-linebreak-by-spaces?
;; 判断位置 pos 处换行前是否至少有两个连续空格。
;;
;; 语法
;; ----
;; (hard-linebreak-by-spaces? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 换行符位置。
;;
;; 返回值
;; ----
;; boolean
;; 满足两空格硬换行返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline escape))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (hard-linebreak-by-spaces? "a  \nb" 3))
(check-false (hard-linebreak-by-spaces? "a \nb" 2))

(check-report)
