;; alert-block-start?
;; 判断行 line 是否为 Alert 块起始标记。
;;
;; 语法
;; ----
;; (alert-block-start? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是 Alert 起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block alert))

(check-set-mode! 'report-failed)

(check-true (alert-block-start? "> [!NOTE]"))
(check-false (alert-block-start? "> normal quote"))

(check-report)
