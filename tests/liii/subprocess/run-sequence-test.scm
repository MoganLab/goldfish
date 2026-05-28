(import (liii check) (liii os) (liii subprocess))

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
;; integer
;; 最后一条命令的退出码。

(when (os-linux?)
  (check (run-sequence "true" "false" "true") => 0)
  (check (run-sequence "false" "true") => 0)
  (check (run-sequence "true" "false") => 1)
  (check (run-sequence "true" "true" :cwd "/tmp") => 0)
)

(check-report)
