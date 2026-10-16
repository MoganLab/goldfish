(import (liii check) (liii either) (liii os) (liii subprocess))

;; run-or
;; 顺序执行命令，成功即停（替代 shell 的 ||）。
;;
;; 语法
;; ----
;; (run-or command ...)
;; (run-or command ... keyword value ...)
;;
;; 参数
;; ----
;; command ... : 多个命令。
;;
;; keyword value ... : 可选
;; :cwd、:env、:timeout — 共享选项。
;;
;; 返回值
;; ----
;; Either
;; 任一命令成功时返回 Right 0，全部失败时返回 Left（内含 <process-error> 记录）。

(when (or (os-linux?) (os-macos?))
  (check (either-right? (run-or "true" "false")) => #t)
  (check (to-right (run-or "true" "false")) => 0)
  (let ((res (run-or "false" "false")))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => "false")
    ) ;let
  ) ;let
  (check (either-right? (run-or "false" "true")) => #t)
  (check (to-right (run-or "false" "true")) => 0)
  (check (either-right? (run-or "true" "false" :cwd "/tmp")) => #t)
  (check (to-right (run-or "true" "false" :cwd "/tmp")) => 0)
) ;when

(when (os-windows?)
  (check (either-right? (run-or "python3 -c pass" "python3 -c 1/0")) => #t)
  (check (to-right (run-or "python3 -c pass" "python3 -c 1/0")) => 0)
  (let ((res (run-or "python3 -c 1/0" "python3 -c 1/0")))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => "python3 -c 1/0")
    ) ;let
  ) ;let
  (check (either-right? (run-or "python3 -c 1/0" "python3 -c pass")) => #t)
  (check (to-right (run-or "python3 -c 1/0" "python3 -c pass")) => 0)
  (check (either-right? (run-or "python3 -c pass" "python3 -c 1/0" :cwd (os-temp-dir)))
    =>
    #t
  ) ;check
  (check (to-right (run-or "python3 -c pass" "python3 -c 1/0" :cwd (os-temp-dir)))
    =>
    0
  ) ;check
) ;when

(check-report)
