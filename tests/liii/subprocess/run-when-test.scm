(import (liii check) (liii either) (liii os) (liii subprocess))

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
;; Either
;; condition 满足时返回 Right 0；command 成功时返回 Right（内含退出码），
;; 失败时返回 Left（内含 (list code command)）。

(when (os-linux?)
  (check (either-right? (run-when "false" "echo yes")) => #t)
  (check (to-right (run-when "false" "echo yes")) => 0)
  (check (either-right? (run-when "true" "echo yes")) => #t)
  (check (to-right (run-when "true" "echo yes")) => 0)

  (check (either-left? (run-when "false" "false")) => #t)
  (check (to-left (run-when "false" "false")) => '(1 "false"))
) ;when

(when (os-windows?)
  (check (either-right? (run-when "python3 -c 1/0" "python3 -c pass")) => #t)
  (check (to-right (run-when "python3 -c 1/0" "python3 -c pass")) => 0)
  (check (either-right? (run-when "python3 -c pass" "python3 -c pass")) => #t)
  (check (to-right (run-when "python3 -c pass" "python3 -c pass")) => 0)

  (check (either-left? (run-when "python3 -c 1/0" "python3 -c 1/0")) => #t)
  (check (to-left (run-when "python3 -c 1/0" "python3 -c 1/0")) => '(1 "python3 -c 1/0"))
) ;when

(check-report)
