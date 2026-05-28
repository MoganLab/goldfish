(import (liii check) (liii os) (liii subprocess))

;; run-when
;; 条件不满足时执行命令。
;;
;; 语法
;; ----
;; (run-when condition-command command)
;;
;; 参数
;; ----
;; condition-command : string 或 list
;; 返回非 0 时执行 command。
;;
;; command : string 或 list
;;
;; 返回值
;; ----
;; integer
;; 0（条件满足时不执行），或 command 的退出码。

(when (os-linux?)
  (check (run-when "false" "echo yes") => 0)
  (check (run-when "true" "echo yes") => 0)
)

(check-report)
