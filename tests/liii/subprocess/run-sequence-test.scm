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
;; 最后一条命令成功时返回 Right（内含退出码），
;; 失败时返回 Left（内含退出码）。

(when (os-linux?)
  (check (either-right? (run-sequence "true" "false" "true")) => #t)
  (check (to-right (run-sequence "true" "false" "true")) => 0)
  (check (either-right? (run-sequence "false" "true")) => #t)
  (check (to-right (run-sequence "false" "true")) => 0)
  (check (either-left? (run-sequence "true" "false")) => #t)
  (check (to-left (run-sequence "true" "false")) => 1)
  (check (either-right? (run-sequence "true" "true" :cwd "/tmp")) => #t)
  (check (to-right (run-sequence "true" "true" :cwd "/tmp")) => 0)
) ;when

(check-report)
