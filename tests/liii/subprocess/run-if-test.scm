(import (liii check) (liii either) (liii os) (liii subprocess))

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
;; Either
;; 被执行命令成功时返回 Right（内含退出码），失败时返回 Left（内含 (list code command)）。
;; condition-command 返回非零且无 else-command 时，返回 Right（内含 condition 的退出码）。

(when (os-linux?)
  (check (either-right? (run-if "true" "echo yes")) => #t)
  (check (to-right (run-if "true" "echo yes")) => 0)
  (check (either-right? (run-if "false" "echo yes")) => #t)
  (check (to-right (run-if "false" "echo yes")) => 1)
  (check (either-right? (run-if "false" "echo yes" "echo no")) => #t)
  (check (to-right (run-if "false" "echo yes" "echo no")) => 0)

  (check (either-left? (run-if "true" "false")) => #t)
  (check (to-left (run-if "true" "false")) => '(1 "false"))
  (check (either-left? (run-if "false" "echo yes" "false")) => #t)
  (check (to-left (run-if "false" "echo yes" "false")) => '(1 "false"))
)

(check-report)
