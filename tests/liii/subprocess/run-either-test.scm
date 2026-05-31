(import (liii check)
  (liii either)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (scheme file)
) ;import

;; run-either
;; 执行命令并返回标准输出或错误信息。
;;
;; 语法
;; ----
;; (run-either command)
;; (run-either command keyword value ...)
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
;; 失败时返回 Left（内含 (cons exit-code stderr-string)）。

(when (os-linux?)
  ;; basic success
  (check (either-right? (run-either "echo hello")) => #t)
  (check (to-right (run-either "echo hello")) => "hello\n")
  (check (either-right? (run-either '(echo "hello"))) => #t)
  (check (to-right (run-either '(echo "hello"))) => "hello\n")

  ;; basic failure
  (check (either-left? (run-either "false")) => #t)
  (let ((result (to-left (run-either "false"))))
    (check (car result) => 1)
    (check (string? (cdr result)) => #t)
  )

  ;; failure with stderr
  (check (either-left? (run-either "ls /nonexistent")) => #t)
  (let ((result (to-left (run-either "ls /nonexistent"))))
    (check (number? (car result)) => #t)
    (check (string? (cdr result)) => #t)
    (check (> (string-length (cdr result)) 0) => #t)
  )

  ;; :cwd
  (check (either-right? (run-either "pwd" :cwd "/tmp")) => #t)
  (check (to-right (run-either "pwd" :cwd "/tmp")) => "/tmp\n")

  ;; :env
  (check (either-right? (run-either "echo $FOO" :env '(("FOO" . "bar")))) => #t)
  (check (to-right (run-either "echo $FOO" :env '(("FOO" . "bar")))) => "bar\n")

  ;; :input
  (check (either-right? (run-either "cat" :input "hello world")) => #t)
  (check (to-right (run-either "cat" :input "hello world")) => "hello world")

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-either-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    )
    (check (either-right? (run-either "echo hello" :stdout tmpfile)) => #t)
    (check (to-right (run-either "echo hello" :stdout tmpfile)) => "")
    (check (path-read-text tmpfile) => "hello\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    )
  )

  ;; :stdout 'discard
  (check (either-right? (run-either "echo hello" :stdout 'discard)) => #t)
  (check (to-right (run-either "echo hello" :stdout 'discard)) => "")

  ;; :stderr 'stdout
  (check (either-right? (run-either "echo hello >&2" :stderr 'stdout)) => #t)
  (check (to-right (run-either "echo hello >&2" :stderr 'stdout)) => "hello\n")

  ;; :stderr 'discard
  (check (either-right? (run-either "echo hello >&2" :stderr 'discard)) => #t)
  (check (to-right (run-either "echo hello >&2" :stderr 'discard)) => "")

  ;; list form with symbol head
  (check (either-right? (run-either '(printf "%s" "hello world"))) => #t)
  (check (to-right (run-either '(printf "%s" "hello world"))) => "hello world")
) ;when

(check-report)
