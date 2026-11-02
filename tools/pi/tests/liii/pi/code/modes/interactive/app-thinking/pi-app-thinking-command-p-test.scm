;; pi-app-thinking-command?
;; 判定输入文本是否为 /thinking 命令。
;;
;; 语法
;; ----
;; (pi-app-thinking-command? text)
;;
;; 参数
;; ----
;; text : string?
;; 用户输入文本。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive app-thinking)
) ;import

(check-set-mode! 'report-failed)

;; 1. 匹配 /thinking 与 /thinking <level>
(check-true (pi-app-thinking-command? "/thinking"))
(check-true (pi-app-thinking-command? "/thinking high"))

;; 2. 不匹配其他文本
(check-false (pi-app-thinking-command? "/think"))
(check-false (pi-app-thinking-command? "thinking"))

(check-report)
