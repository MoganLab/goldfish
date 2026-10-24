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
;; condition 满足时返回 Right 0；command 成功时返回 Right 0，
;; 失败时返回 Left（内含 <process-error> 记录）。

(when (or (os-linux?) (os-macos?))
  (check (either-right? (run-when "false" "echo yes")) => #t)
  (check (to-right (run-when "false" "echo yes")) => 0)
  (check (either-right? (run-when "true" "echo yes")) => #t)
  (check (to-right (run-when "true" "echo yes")) => 0)

  (let ((res (run-when "false" "false")))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => "false")
    ) ;let
  ) ;let
) ;when

(when (os-windows?)
  (check (either-right? (run-when "python3 -c 1/0" "python3 -c pass")) => #t)
  (check (to-right (run-when "python3 -c 1/0" "python3 -c pass")) => 0)
  (check (either-right? (run-when "python3 -c pass" "python3 -c pass")) => #t)
  (check (to-right (run-when "python3 -c pass" "python3 -c pass")) => 0)

  (let ((res (run-when "python3 -c 1/0" "python3 -c 1/0")))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => "python3 -c 1/0")
    ) ;let
  ) ;let
) ;when

(check-report)
