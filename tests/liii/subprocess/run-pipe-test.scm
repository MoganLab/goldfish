(import (liii check) (liii either) (liii os) (liii subprocess))

;; run-pipe
;; 将多个命令串联为管道（替代 shell 的 |）。
;;
;; 语法
;; ----
;; (run-pipe command ...)
;; (run-pipe command ... keyword value ...)
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
;; 成功时返回 Right（内含管道最后一级的 stdout 字符串），
;; 失败时返回 Left（内含 (list code command)）。

(when (os-linux?)
  (check (either-right? (run-pipe "echo hello world" '("grep" "hello"))) => #t)
  (check (to-right (run-pipe "echo hello world" '("grep" "hello"))) => "hello world\n")
  (check (either-right? (run-pipe '("printf" "a\nb\nc") '("grep" "a") '("wc" "-l"))) => #t)
  (check (to-right (run-pipe '("printf" "a\nb\nc") '("grep" "a") '("wc" "-l"))) => "1\n")
  (check (either-right? (run-pipe "echo hello")) => #t)
  (check (to-right (run-pipe "echo hello")) => "hello\n")

  (check (either-left? (run-pipe "echo hello" '("false"))) => #t)
  (check (to-left (run-pipe "echo hello" '("false"))) => '(1 ("false")))
)

(check-report)
