;; pi-app-session-command?
;; 判定输入文本是否为 /session 命令。
;;
;; 语法
;; ----
;; (pi-app-session-command? text)
;;
;; 参数
;; ----
;; text : string?
;; 用户输入的命令文本。
;;
;; 返回值
;; ----
;; boolean?
;; 精确匹配 "/session" 时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive app-session))

(check-set-mode! 'report-failed)

;; 1. 精确匹配 /session
(check-true (pi-app-session-command? "/session"))

;; 2. 不匹配带参数或其他命令
(check-false (pi-app-session-command? "/session extra"))
(check-false (pi-app-session-command? "/sess"))
(check-false (pi-app-session-command? "session"))

(check-report)
