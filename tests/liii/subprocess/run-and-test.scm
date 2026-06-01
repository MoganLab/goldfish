(import (liii check)
  (liii either)
  (liii os)
  (liii path)
  (liii subprocess)
  (scheme file)
) ;import

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
;; Either
;; 全部成功时返回 Right（内含最后一个命令的退出码），
;; 失败时返回 Left（内含 (list code command)）。

(when (os-linux?)
  (check (either-right? (run-and "true" "true")) => #t)
  (check (to-right (run-and "true" "true")) => 0)
  (check (either-left? (run-and "false" "true")) => #t)
  (check (to-left (run-and "false" "true")) => '(1 "false"))
  (check (either-left? (run-and "true" "false")) => #t)
  (check (to-left (run-and "true" "false")) => '(1 "false"))
  (check (either-right? (run-and "true" "true" :cwd "/tmp")) => #t)
  (check (to-right (run-and "true" "true" :cwd "/tmp")) => 0)
  (check (either-right? (run-and '(sh "-c" "test $FOO = bar") :env '(("FOO" . "bar"))))
    =>
    #t
  ) ;check
  (check (to-right (run-and '(sh "-c" "test $FOO = bar") :env '(("FOO" . "bar")))) => 0)

  ;; :stdout only on last
  (let ((tmpfile "/tmp/gf-run-and-stdout.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (either-right? (run-and "echo a" "echo b" :stdout tmpfile)) => #t)
    (check (to-right (run-and "echo a" "echo b" :stdout tmpfile)) => 0)
    (check (path-read-text tmpfile) => "b\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :input only on first
  (check (either-right? (run-and "cat" "true" :input "hello")) => #t)
  (check (to-right (run-and "cat" "true" :input "hello")) => 0)

  ;; Multiple commands
  (check (either-right? (run-and "true" "true" "true")) => #t)
  (check (to-right (run-and "true" "true" "true")) => 0)
  (check (either-left? (run-and "true" "false" "true")) => #t)
  (check (to-left (run-and "true" "false" "true")) => '(1 "false"))
) ;when

(when (os-windows?)
  (check (either-right? (run-and "python3 -c pass" "python3 -c pass")) => #t)
  (check (to-right (run-and "python3 -c pass" "python3 -c pass")) => 0)
  (check (either-left? (run-and "python3 -c 1/0" "python3 -c pass")) => #t)
  (check (to-left (run-and "python3 -c 1/0" "python3 -c pass")) => '(1 "python3 -c 1/0"))
  (check (either-left? (run-and "python3 -c pass" "python3 -c 1/0")) => #t)
  (check (to-left (run-and "python3 -c pass" "python3 -c 1/0")) => '(1 "python3 -c 1/0"))
  (check (either-right? (run-and "python3 -c pass" "python3 -c pass" :cwd (os-temp-dir))) => #t)
  (check (to-right (run-and "python3 -c pass" "python3 -c pass" :cwd (os-temp-dir))) => 0)
  (let ((path-env (getenv "PATH")))
    (check (either-right? (run-and "python3 -c pass" :env `(("FOO" . "bar") ("PATH" . ,path-env)))) => #t)
    (check (to-right (run-and "python3 -c pass" :env `(("FOO" . "bar") ("PATH" . ,path-env)))) => 0))

  ;; :stdout only on last
  (let ((tmpfile (string-append (os-temp-dir) "/gf-run-and-stdout-win.txt")))
    (when (file-exists? tmpfile) (delete-file tmpfile))
    (check (either-right? (run-and "python3 -c pass" "python3 -c pass" :stdout tmpfile)) => #t)
    (check (to-right (run-and "python3 -c pass" "python3 -c pass" :stdout tmpfile)) => 0)
    (when (file-exists? tmpfile) (delete-file tmpfile))
  ) ;let

  ;; Multiple commands
  (check (either-right? (run-and "python3 -c pass" "python3 -c pass" "python3 -c pass")) => #t)
  (check (to-right (run-and "python3 -c pass" "python3 -c pass" "python3 -c pass")) => 0)
  (check (either-left? (run-and "python3 -c pass" "python3 -c 1/0" "python3 -c pass")) => #t)
  (check (to-left (run-and "python3 -c pass" "python3 -c 1/0" "python3 -c pass")) => '(1 "python3 -c 1/0"))
) ;when

(check-report)
