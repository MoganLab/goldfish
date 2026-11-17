;; bare-autolink-start?
;; 判断位置 pos 是否为合法裸链接起始。
;;
;; 语法
;; ----
;; (bare-autolink-start? str pos)
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
;; 匹配裸链接返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline autolink))

(check-set-mode! 'report-failed)

(check-true (bare-autolink-start? "https://example.com" 0))
(check-false (bare-autolink-start? "hello" 0))

(check-report)
