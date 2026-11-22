;; backslash-hard-linebreak?
;; 判断位置 pos 是否为反斜杠加换行构成的硬换行。
;;
;; 语法
;; ----
;; (backslash-hard-linebreak? str pos)
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
;; 构成硬换行返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline escape))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (backslash-hard-linebreak? "\\\n" 0))
(check-false (backslash-hard-linebreak? "\\*" 0))

(check-report)
