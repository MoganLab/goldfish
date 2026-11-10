;; %check-symbol-command
;; 校验符号命令是否允许执行（检查黑名单与白名单）。
;;
;; 语法
;; ----
;; (%check-symbol-command sym)
;;
;; 参数
;; ----
;; sym : symbol
;; 待校验的命令符号。
;;
;; 返回值
;; ------
;; 未定义
;;
;; 说明
;; ----
;; - 若符号处于黑名单列表（如默认的 'rm），抛出 value-error。
;; - 若设置了白名单且符号不在白名单中，抛出 value-error。
;; - 校验通过时正常返回。

(import (liii check) (liii subprocess registry))

(check-set-mode! 'report-failed)

;; 默认黑名单包含 'rm
(check-catch 'value-error (%check-symbol-command 'rm))

;; 未被禁止的符号默认放行
(%check-symbol-command 'ls)
(%check-symbol-command 'echo)

;; 测试白名单控制
(run-allow! 'echo)
(%check-symbol-command 'echo)
(check-catch 'value-error (%check-symbol-command 'ls))

;; 恢复白名单为空
(run-allow! '())
(%check-symbol-command 'ls)

;; 测试动态添加黑名单
(run-ban! 'forbidden-cmd)
(check-catch 'value-error (%check-symbol-command 'forbidden-cmd))
(run-unban! 'forbidden-cmd)
(%check-symbol-command 'forbidden-cmd)

(check-report)
