(import (liii check) (liii os) (liii subprocess))

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
;; string
;; 管道最后一级的 stdout 字符串。
;;
;; 说明
;; ----
;; 基于内存缓冲实现：前一级 stdout 作为后一级的 :input 传递。

(when (os-linux?)
  (check (run-pipe "echo hello world" '("grep" "hello")) => "hello world\n")
  (check (run-pipe "echo a\nb\nc" '("grep" "a") '("wc" "-l")) => "1\n")
  (check (run-pipe "echo hello") => "hello\n")
)

(check-report)
