;; %check-string-command
;; 校验字符串命令是否匹配黑名单前缀。
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
;; ------
;; 未定义
;;
;; 说明
;; ----
;; - 若命令字符串以黑名单中的命令符号开头且紧跟空格或直接结束，抛出 value-error。
;; - 包含相同前缀但非空格词边界的命令（例如 rmdir）正常放行。
;; - 未在黑名单中的命令正常放行。

(import (liii check) (liii subprocess registry))

(check-set-mode! 'report-failed)

;; 默认黑名单 'rm
(check-catch 'value-error (%check-string-command "rm"))
(check-catch 'value-error (%check-string-command "rm -rf /tmp/test"))
(check-catch 'value-error (%check-string-command "rm file.txt"))

;; 相同前缀但非空格分隔，正常放行
(%check-string-command "rmdir /tmp/test")
(%check-string-command "rm_file")

;; 普通命令放行
(%check-string-command "ls -la")
(%check-string-command "echo hello")

;; 动态添加黑名单
(run-ban! 'custom-ban)
(check-catch 'value-error (%check-string-command "custom-ban"))
(check-catch 'value-error (%check-string-command "custom-ban --flag"))
(%check-string-command "custom-ban-all")
(run-unban! 'custom-ban)
(%check-string-command "custom-ban")

(check-report)
