(import (liii check) (liii os) (liii subprocess))

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
;; integer
;; 0（任一命令成功），或最后一个失败命令的退出码。

(when (os-linux?)
  (check (run-or "true" "false") => 0)
  (check (run-or "false" "false") => 1)
  (check (run-or "false" "true") => 0)
  (check (run-or "true" "false" :cwd "/tmp") => 0)
)

(check-report)
