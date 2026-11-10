;; pi-bash-execution->user-text
;; 构造注入上下文的 Bash 执行结果文本。
;;
;; 语法
;; ----
;; (pi-bash-execution->user-text command output exit-code)
;;
;; 参数
;; ----
;; command : string?
;; 执行的 shell 命令。
;;
;; output : string?
;; 命令标准输出与错误输出文本。
;;
;; exit-code : integer?
;; 命令退出码；非零时追加退出码说明。
;;
;; 返回值
;; ----
;; string?
;; 格式化后的用户消息文本。

(import (scheme base) (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-bash-execution->user-text "ls" "a\nb" 0)
  =>
  "Ran `ls`\n```\na\nb\n```"
) ;check
(check (pi-bash-execution->user-text "true" "" 0) => "Ran `true`\n(no output)")
(check (pi-bash-execution->user-text "false" "boom" 1)
  =>
  "Ran `false`\n```\nboom\n```\n\nCommand exited with code 1"
) ;check

(check-report)
