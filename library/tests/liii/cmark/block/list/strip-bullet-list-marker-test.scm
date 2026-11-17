;; strip-bullet-list-marker
;; 剥除无序列表项前缀标记。
;;
;; 语法
;; ----
;; (strip-bullet-list-marker line)
;;
;; 参数
;; ----
;; line : string
;; 无序列表项行。
;;
;; 返回值
;; ----
;; string
;; 返回内容字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (strip-bullet-list-marker "- hello") => "hello")

(check-report)
