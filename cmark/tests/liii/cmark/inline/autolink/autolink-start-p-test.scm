;; autolink-start?
;; 判断位置 pos 是否为尖括号自动链接起始。
;;
;; 语法
;; ----
;; (autolink-start? str pos)
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
;; 匹配自动链接返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline autolink))

(check-set-mode! 'report-failed)

(check-true (autolink-start? "<https://example.com>" 0))
(check-false (autolink-start? "<not a link>" 0))

(check-report)
