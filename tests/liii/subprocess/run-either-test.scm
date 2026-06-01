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
;; :stdout — 'capture、'discard 或文件路径。
;; :stderr — 'capture、'stdout、'discard 或文件路径。
;; :stdin — 文件路径或 'null。
;;
;; 返回值
;; ----
;; Either
;; 成功时返回 Right（内含标准输出字符串），
;; 失败时返回 Left（内含 (cons exit-code stderr-string)）。

(when (os-linux?)
  ;; basic success
  (check (either-right? (run-either "echo hello" :stdout 'capture)) => #t)
  (check (to-right (run-either "echo hello" :stdout 'capture)) => "hello\n")
  (check (either-right? (run-either '(echo "hello") :stdout 'capture)) => #t)
  (check (to-right (run-either '(echo "hello") :stdout 'capture)) => "hello\n")

  ;; basic failure
  (check (either-left? (run-either "false")) => #t)
  (let ((result (to-left (run-either "false"))))
    (check (car result) => 1)
    (check (string? (cdr result)) => #t)
  )

  ;; failure with stderr
  (check (either-left? (run-either "ls /nonexistent" :stderr 'capture)) => #t)
  (let ((result (to-left (run-either "ls /nonexistent" :stderr 'capture))))
    (check (number? (car result)) => #t)
    (check (string? (cdr result)) => #t)
    (check (> (string-length (cdr result)) 0) => #t)
  )

  ;; :cwd
  (check (either-right? (run-either "pwd" :cwd "/tmp" :stdout 'capture)) => #t)
  (check (to-right (run-either "pwd" :cwd "/tmp" :stdout 'capture)) => "/tmp\n")

  ;; :env
  (let ((result (run-either '(env) :env '(("FOO" . "bar")) :stdout 'capture)))
    (check (either-right? result) => #t)
    (check (string-contains? (to-right result) "FOO=bar") => #t))

  ;; :input
  (check (either-right? (run-either "cat" :input "hello world" :stdout 'capture)) => #t)
  (check (to-right (run-either "cat" :input "hello world" :stdout 'capture)) => "hello world")

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
  (check (either-right? (run-either "echo hello" :stderr 'stdout :stdout 'capture)) => #t)
  (check (to-right (run-either "echo hello" :stderr 'stdout :stdout 'capture)) => "hello\n")

  ;; :stderr 'discard
  (check (either-right? (run-either "echo hello" :stderr 'discard)) => #t)
  (check (to-right (run-either "echo hello" :stderr 'discard)) => "hello\n")

  ;; list form with symbol head
  (check (either-right? (run-either '(printf "%s" "hello world") :stdout 'capture)) => #t)
  (check (to-right (run-either '(printf "%s" "hello world") :stdout 'capture)) => "hello world")
) ;when

(when (os-windows?)
  ;; basic success
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-out-win.txt")))
    (when (file-exists? tmpfile) (delete-file tmpfile))
    (check (either-right? (run-either "python3 -c pass" :stdout tmpfile :stderr 'discard)) => #t)
    (check (to-right (run-either "python3 -c pass" :stdout tmpfile :stderr 'discard)) => "")
    (when (file-exists? tmpfile) (delete-file tmpfile))
  ) ;let

  ;; basic failure
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-out2-win.txt")))
    (when (file-exists? tmpfile) (delete-file tmpfile))
    (check (either-left? (run-either "python3 -c 1/0" :stdout tmpfile :stderr 'discard)) => #t)
    (let ((result (to-left (run-either "python3 -c 1/0" :stdout tmpfile :stderr 'discard))))
      (check (number? (car result)) => #t)
      (check (string? (cdr result)) => #t))
    (when (file-exists? tmpfile) (delete-file tmpfile))
  ) ;let

  ;; :cwd
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-cwd-win.txt")))
    (when (file-exists? tmpfile) (delete-file tmpfile))
    (check (either-right? (run-either "python3 -c pass" :cwd (os-temp-dir) :stdout tmpfile :stderr 'discard)) => #t)
    (when (file-exists? tmpfile) (delete-file tmpfile))
  ) ;let

  ;; :env
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-env-win.txt"))
        (path-env (getenv "PATH")))
    (when (file-exists? tmpfile) (delete-file tmpfile))
    (check (either-right? (run-either "python3 -c pass" :env `(("FOO" . "bar") ("PATH" . ,path-env)) :stdout tmpfile :stderr 'discard)) => #t)
    (when (file-exists? tmpfile) (delete-file tmpfile))
  ) ;let

  ;; :stdout to file
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-file-win.txt")))
    (when (file-exists? tmpfile) (delete-file tmpfile))
    (check (either-right? (run-either "python3 -c print('hello')" :stdout tmpfile :stderr 'discard)) => #t)
    (check (to-right (run-either "python3 -c print('hello')" :stdout tmpfile :stderr 'discard)) => "")
    (check (path-read-text tmpfile) => "hello\n")
    (when (file-exists? tmpfile) (delete-file tmpfile))
  ) ;let

  ;; :stdout 'discard
  (check (either-right? (run-either "python3 -c pass" :stdout 'discard :stderr 'discard)) => #t)
  (check (to-right (run-either "python3 -c pass" :stdout 'discard :stderr 'discard)) => "")

  ;; list form with symbol head
  (run-set! 'pypass "python3")
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-either-list-win.txt")))
    (when (file-exists? tmpfile) (delete-file tmpfile))
    (check (either-right? (run-either '(pypass "-c" "print('hello world')") :stdout tmpfile :stderr 'discard)) => #t)
    (check (to-right (run-either '(pypass "-c" "print('hello world')") :stdout tmpfile :stderr 'discard)) => "")
    (when (file-exists? tmpfile) (delete-file tmpfile))
  ) ;let
) ;when

(check-report)
