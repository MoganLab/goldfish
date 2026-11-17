;; emph-marker-at?
;; 判断位置 pos 是否为 * 或 _ 强调标记。
;;
;; 语法
;; ----
;; (emph-marker-at? str pos)
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
;; 是强调标记返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-delimiters))

(check-set-mode! 'report-failed)

(check-true (emph-marker-at? "*a" 0))
(check-false (emph-marker-at? "ab" 0))

(check-report)
