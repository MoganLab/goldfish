(import (liii check) (liii os) (liii path) (liii subprocess) (scheme file))

;; run-and
;; 顺序执行命令，失败即停（替代 shell 的 &&）。
;;
;; 语法
;; ----
;; (run-and command ...)
;; (run-and command ... keyword value ...)
;;
;; 参数
;; ----
;; command ... : 多个命令（字符串或列表）。
;;
;; keyword value ... : 可选
;; :cwd、:env、:timeout — 共享选项，作用到每个命令。
;; :stdout、:stderr — 只对最后一个命令生效。
;; :input、:stdin — 只对第一个命令生效。
;;
;; 返回值
;; ----
;; integer
;; 第一个失败命令的退出码，或 0（全部成功）。

(when (os-linux?)
  (check (run-and "true" "true") => 0)
  (check (run-and "false" "true") => 1)
  (check (run-and "true" "false") => 1)
  (check (run-and "true" "true" :cwd "/tmp") => 0)
  (check (run-and "test $FOO = bar" :env '(("FOO" . "bar"))) => 0)

  ;; :stdout only on last
  (let ((tmpfile "/tmp/gf-run-and-stdout.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    )
    (check (run-and "echo a" "echo b" :stdout tmpfile) => 0)
    (check (path-read-text tmpfile) => "b\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    )
  )

  ;; :input only on first
  (check (run-and "cat" "true" :input "hello") => 0)

  ;; Multiple commands
  (check (run-and "true" "true" "true") => 0)
  (check (run-and "true" "false" "true") => 1)
)

(check-report)
