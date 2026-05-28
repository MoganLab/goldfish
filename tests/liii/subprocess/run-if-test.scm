(import (liii check) (liii os) (liii subprocess))

;; run-if
;; 条件执行命令（替代 shell 的 if test）。
;;
;; 语法
;; ----
;; (run-if condition-command then-command)
;; (run-if condition-command then-command else-command)
;;
;; 参数
;; ----
;; condition-command : string 或 list
;; 返回 0 时执行 then-command，否则执行 else-command（如果有）。
;;
;; then-command、else-command : string 或 list
;;
;; 返回值
;; ----
;; integer
;; 被执行命令的退出码，或 condition-command 的退出码（无 else 且条件为假时）。

(when (os-linux?)
  (check (run-if "true" "echo yes") => 0)
  (check (run-if "false" "echo yes") => 1)
  (check (run-if "false" "echo yes" "echo no") => 0)
)

(check-report)
