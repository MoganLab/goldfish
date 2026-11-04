;; %check-string-command
;; 检查字符串命令是否命中黑名单。
;;
;; 语法
;; ----
;; (%check-string-command cmd)
;;
;; 参数
;; ----
;; cmd : string
;; 待检查的命令行字符串。
;;
;; 返回值
;; ----
;; 未定义
;;
;; 说明
;; ----
;; 若命令命中黑名单（例如以 "rm" 开头且后接空格或结束），则抛出 value-error；
;; 否则正常返回。

(import (liii check) (liii error) (liii subprocess registry))

(check-set-mode! 'report-failed)

;; 1. 默认黑名单中 rm 命令命中时抛出 value-error
(check-catch 'value-error (%check-string-command "rm"))
(check-catch 'value-error (%check-string-command "rm -rf /"))
(check-catch 'value-error (%check-string-command "rm -rf /tmp/test"))

;; 2. 以 rm 开头但为独立命令名的字符串不被误判放行
(check-true (begin (%check-string-command "rmdir foo") #t))
(check-true (begin (%check-string-command "rmdir /tmp/dir") #t))

;; 3. 普通安全命令正常放行
(check-true (begin (%check-string-command "echo hello") #t))
(check-true (begin (%check-string-command "ls -la") #t))

(check-report)
