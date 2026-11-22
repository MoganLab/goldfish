;; bare-autolink-candidate-start?
;; 快速判断位置 pos 是否可能是裸链接候选起点。
;;
;; 语法
;; ----
;; (bare-autolink-candidate-start? str pos)
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
;; 可能是候选返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline autolink))

(check-set-mode! 'report-failed)

(check-true (bare-autolink-candidate-start? "https://a.b" 0))
(check-false (bare-autolink-candidate-start? "abc" 0))

(check-report)
