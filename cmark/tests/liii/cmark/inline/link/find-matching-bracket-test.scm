;; find-matching-bracket
;; 从 [ 位置起查找与之匹配的 ] 位置。
;;
;; 语法
;; ----
;; (find-matching-bracket str start-pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start-pos : integer
;; [ 起始位置。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 找到返回 ] 的位置，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link))

(check-set-mode! 'report-failed)

(check (find-matching-bracket "[a [b] c]" 0) => 8)
(check (find-matching-bracket "[unclosed" 0) => #f)

(check-report)
