(import (liii check) (liii either) (liii os) (liii subprocess))

;; run-sequence
;; 顺序执行所有命令，不因为失败而中断（替代 shell 的 ;）。
;;
;; 语法
;; ----
;; (run-sequence command ...)
;; (run-sequence command ... keyword value ...)
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
;; 最后一条命令成功时返回 Right（内含退出码 0），
;; 失败时返回 Left（内含 <process-error> 记录）。

(when (or (os-linux?) (os-macos?))
  (check (either-right? (run-sequence "true" "false" "true")) => #t)
  (check (to-right (run-sequence "true" "false" "true")) => 0)
  (check (either-right? (run-sequence "false" "true")) => #t)
  (check (to-right (run-sequence "false" "true")) => 0)
  (let ((res (run-sequence "true" "false")))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => "false")
    ) ;let
  ) ;let
  (check (either-right? (run-sequence "true" "true" :cwd "/tmp")) => #t)
  (check (to-right (run-sequence "true" "true" :cwd "/tmp")) => 0)
) ;when

(when (os-windows?)
  (check (either-right? (run-sequence "python3 -c pass" "python3 -c 1/0" "python3 -c pass")
         ) ;either-right?
    =>
    #t
  ) ;check
  (check (to-right (run-sequence "python3 -c pass" "python3 -c 1/0" "python3 -c pass"))
    =>
    0
  ) ;check
  (check (either-right? (run-sequence "python3 -c 1/0" "python3 -c pass")) => #t)
  (check (to-right (run-sequence "python3 -c 1/0" "python3 -c pass")) => 0)
  (let ((res (run-sequence "python3 -c pass" "python3 -c 1/0")))
    (check (either-left? res) => #t)
    (let ((err (to-left res)))
      (check (process-error? err) => #t)
      (check (process-error-code err) => 1)
      (check (process-error-command err) => "python3 -c 1/0")
    ) ;let
  ) ;let
  (check (either-right? (run-sequence "python3 -c pass" "python3 -c pass" :cwd (os-temp-dir))
         ) ;either-right?
    =>
    #t
  ) ;check
  (check (to-right (run-sequence "python3 -c pass" "python3 -c pass" :cwd (os-temp-dir)))
    =>
    0
  ) ;check
) ;when

(check-report)
