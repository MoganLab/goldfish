(import (liii check) (liii os) (liii path) (liii subprocess) (scheme file))

;; run-values
;; 执行命令并多值返回 stdout、stderr 和退出码。
;;
;; 语法
;; ----
;; (run-values command)
;; (run-values command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;;
;; keyword value ... : 可选
;; :cwd — 工作目录。
;; :env — 环境变量覆盖，格式为 '(("KEY" . "value") ...)。
;; :input — 写入子进程 stdin 的字符串。
;; :timeout — 超时时间（秒），超时后返回 -1。
;; :stdout — 'capture、'discard 或文件路径。
;; :stderr — 'capture、'stdout、'discard 或文件路径。
;; :stdin — 文件路径或 'null。
;;
;; 返回值
;; ----
;; values : stdout-string stderr-string exit-code
;; exit-code 为 0 表示成功，-1 表示超时。

(when (os-linux?)
  (let-values (((out err code) (run-values "echo hello" :stdout 'capture)))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  (let-values (((out err code) (run-values "false")))
    (check out => "")
    (check (zero? code) => #f)
  ) ;let-values

  ;; :env
  (let-values (((out err code) (run-values "echo $FOO" :env '(("FOO" . "bar")) :stdout 'capture)))
    (check out => "bar\n")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :input
  (let-values (((out err code) (run-values "cat" :input "hello world" :stdout 'capture)))
    (check out => "hello world")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :timeout
  (let-values (((out err code) (run-values "sleep 10" :timeout 1)))
    (check code => -1)
  ) ;let-values

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-values-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (let-values (((out err code) (run-values "echo hello" :stdout tmpfile)))
      (check out => "")
      (check (zero? code) => #t)
      (check (path-read-text tmpfile) => "hello\n")
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (let-values (((out err code) (run-values "echo hello" :stdout 'discard)))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'stdout
  (let-values (((out err code) (run-values "echo hello >&2" :stderr 'stdout)))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'discard
  (let-values (((out err code) (run-values "echo hello >&2" :stderr 'discard)))
    (check out => "")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stdin from file
  (let ((tmpfile "/tmp/gf-run-values-stdin-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (call-with-output-file tmpfile (lambda (p) (display "file content\n" p)))
    (let-values (((out err code) (run-values "cat" :stdin tmpfile :stdout 'capture)))
      (check out => "file content\n")
      (check (zero? code) => #t)
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdin 'null
  (let-values (((out err code) (run-values "cat" :stdin 'null :stdout 'capture)))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; list form with symbol head
  (let-values (((out err code) (run-values '(printf "%s" "hello world") :stdout 'capture)))
    (check out => "hello world")
    (check (zero? code) => #t)
  ) ;let-values
) ;when

(check-report)
