;; %check-string-command
;; 校验字符串命令是否命中了黑名单。
;;
;; 语法
;; ----
;; (%check-string-command cmd)
;;
;; 参数
;; ----
;; cmd : string
;; 待校验的命令行字符串。
;;
;; 返回值
;; ----
;; 未定义
;;
;; 说明
;; ----
;; 若命令以黑名单中的命令名称开头（后接空格或直接结束），则抛出 value-error。

(import (liii check) (liii error) (liii subprocess registry))

(check-set-mode! 'report-failed)

;; 1. 默认黑名单中 rm 命令触发 value-error
(check-catch 'value-error (%check-string-command "rm"))
(check-catch 'value-error (%check-string-command "rm -rf /tmp/test"))

;; 2. 以 rm 开头但为独立命令名的字符串不被误判
(check-true (begin (%check-string-command "rmdir foo") #t))
(check-true (begin (%check-string-command "rmdir /tmp/test") #t))

;; 3. 普通安全命令正常通过
(check-true (begin (%check-string-command "echo hello") #t))
(check-true (begin (%check-string-command "true") #t))

(check-report)
