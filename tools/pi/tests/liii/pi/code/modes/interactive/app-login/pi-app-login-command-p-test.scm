;; pi-app-login-command?
;; 判断用户输入文本是否为 /login 命令。
;;
;; 语法
;; ----
;; (pi-app-login-command? text)
;;
;; 参数
;; ----
;; text : string?
;; 用户输入的命令文本。
;;
;; 返回值
;; ----
;; boolean?
;; 是否以 /login 起始。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive app-login)
) ;import

(check-set-mode! 'report-failed)

;; 1. 命令前缀识别
(check-true (pi-app-login-command? "/login"))
(check-true (pi-app-login-command? "/login deepseek"))
(check-true (pi-app-login-command? "/login kimi-coding sk-key"))
(check-false (pi-app-login-command? "/log"))
(check-false (pi-app-login-command? "login"))

(check-report)
