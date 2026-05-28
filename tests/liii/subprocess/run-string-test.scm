(import (liii check) (liii os) (liii path) (liii subprocess) (scheme file))

;; run-string
;; 执行命令并返回标准输出字符串。
;;
;; 语法
;; ----
;; (run-string command)
;; (run-string command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;;
;; keyword value ... : 可选
;; :cwd — 工作目录。
;; :env — 环境变量覆盖。
;; :input — 写入子进程 stdin 的字符串。
;; :timeout — 超时时间（秒）。
;; :stdout — 'discard 或文件路径。
;; :stderr — 'stdout、'discard 或文件路径。
;; :stdin — 文件路径或 'null。
;;
;; 返回值
;; ----
;; string
;; 标准输出字符串（末尾换行保留）。
;;
;; 错误处理
;; ----
;; value-error 当命令返回非 0 退出码时。
;;
;; 说明
;; ----
;; 由于 run-string 不返回退出码，非 0 时抛出异常以避免静默失败。

(when (os-linux?)
  (check (run-string "echo hello") => "hello\n")
  (check (run-string '("echo" "hello")) => "hello\n")

  (check-catch 'value-error (run-string "false"))

  (check (run-string "pwd" :cwd "/tmp") => "/tmp\n")

  ;; :env
  (check (run-string "echo $FOO" :env '(("FOO" . "bar"))) => "bar\n")

  ;; :input
  (check (run-string "cat" :input "hello world") => "hello world")

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-string-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    )
    (check (run-string "echo hello" :stdout tmpfile) => "")
    (check (path-read-text tmpfile) => "hello\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    )
  )

  ;; :stdout 'discard
  (check (run-string "echo hello" :stdout 'discard) => "")

  ;; :stderr 'stdout
  (check (run-string "echo hello >&2" :stderr 'stdout) => "hello\n")

  ;; :stderr 'discard
  (check (run-string "echo hello >&2" :stderr 'discard) => "")

  ;; list form bypasses shell
  (check (run-string '("printf" "%s" "hello world")) => "hello world")
)

(check-report)
