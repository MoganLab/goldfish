;; pi-app-name-command?
;; 判定输入文本是否为 /name 命令。
;;
;; 语法
;; ----
;; (pi-app-name-command? text)
;;
;; 参数
;; ----
;; text : string?
;; 用户输入的命令文本。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive app-name))

(check-set-mode! 'report-failed)

;; 1. 匹配 /name 与 /name <args>
(check-true (pi-app-name-command? "/name"))
(check-true (pi-app-name-command? "/name my-session"))

;; 2. 不匹配其他命令
(check-false (pi-app-name-command? "/nam"))
(check-false (pi-app-name-command? "/namexyz"))
(check-false (pi-app-name-command? "name"))

(check-report)
