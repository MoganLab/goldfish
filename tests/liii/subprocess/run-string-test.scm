(import (liii check)
  (liii either)
  (liii os)
  (liii path)
  (liii subprocess)
  (scheme file)
) ;import

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
;; Either
;; 成功时返回 Right（内含标准输出字符串），
;; 失败时返回 Left（内含退出码）。

(when (os-linux?)
  (check (either-right? (run-string "echo hello")) => #t)
  (check (to-right (run-string "echo hello")) => "hello\n")
  (check (either-right? (run-string '(echo "hello"))) => #t)
  (check (to-right (run-string '(echo "hello"))) => "hello\n")

  (check (either-left? (run-string "false")) => #t)
  (check (to-left (run-string "false")) => 1)

  (check (either-right? (run-string "pwd" :cwd "/tmp")) => #t)
  (check (to-right (run-string "pwd" :cwd "/tmp")) => "/tmp\n")

  ;; :env
  (check (either-right? (run-string "echo $FOO" :env '(("FOO" . "bar")))) => #t)
  (check (to-right (run-string "echo $FOO" :env '(("FOO" . "bar")))) => "bar\n")

  ;; :input
  (check (either-right? (run-string "cat" :input "hello world")) => #t)
  (check (to-right (run-string "cat" :input "hello world")) => "hello world")

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-string-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-right? (run-string "echo hello" :stdout tmpfile)) => #t)
    (check (to-right (run-string "echo hello" :stdout tmpfile)) => "")
    (check (path-read-text tmpfile) => "hello\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (check (either-right? (run-string "echo hello" :stdout 'discard)) => #t)
  (check (to-right (run-string "echo hello" :stdout 'discard)) => "")

  ;; :stderr 'stdout
  (check (either-right? (run-string "echo hello >&2" :stderr 'stdout)) => #t)
  (check (to-right (run-string "echo hello >&2" :stderr 'stdout)) => "hello\n")

  ;; :stderr 'discard
  (check (either-right? (run-string "echo hello >&2" :stderr 'discard)) => #t)
  (check (to-right (run-string "echo hello >&2" :stderr 'discard)) => "")

  ;; list form with symbol head
  (check (either-right? (run-string '(printf "%s" "hello world"))) => #t)
  (check (to-right (run-string '(printf "%s" "hello world"))) => "hello world")
) ;when

(check-report)
