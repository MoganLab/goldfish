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
;; 任一命令成功时返回 Right 0，全部失败时返回 Left（内含 (list code command)）。

(when (os-linux?)
  (check (either-right? (run-or "true" "false")) => #t)
  (check (to-right (run-or "true" "false")) => 0)
  (check (either-left? (run-or "false" "false")) => #t)
  (check (to-left (run-or "false" "false")) => '(1 "false"))
  (check (either-right? (run-or "false" "true")) => #t)
  (check (to-right (run-or "false" "true")) => 0)
  (check (either-right? (run-or "true" "false" :cwd "/tmp")) => #t)
  (check (to-right (run-or "true" "false" :cwd "/tmp")) => 0)
)

(check-report)
