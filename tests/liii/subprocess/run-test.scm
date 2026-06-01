(import (liii check) (liii os) (liii subprocess))

;; run
;; 执行命令并返回退出码。
;;
;; 语法
;; ----
;; (run command)
;; (run command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;; 字符串形式通过 wordexp 拆分后直接执行，列表形式以 symbol 开头。
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
;; integer
;; 退出码，0 表示成功。
;;
;; 说明
;; ----
;; 1. 字符串命令通过 wordexp 拆分后执行，不支持 shell 管道等特性。
;; 2. 列表形式以 symbol 开头，最终转换为字符串执行。
;; 3. 命令中显式包含 cd 时不可同时设置 :cwd，否则抛 value-error。

(when (os-linux?)
  (check (zero? (run "true")) => #t)
  (check (zero? (run "false")) => #f)
  (check (run "echo hello") => 0)
  (check (zero? (run '(true))) => #t)
  (check (zero? (run '(false))) => #f)

  (let ((orig-dir (getcwd)))
    (run "pwd" :cwd "/tmp")
    (check (getcwd) => orig-dir)
  ) ;let

  (let ((orig-dir (getcwd)))
    (run '(pwd) :cwd "/tmp")
    (check (getcwd) => orig-dir)
  ) ;let

  (check-catch 'value-error (run "cd /tmp" :cwd "/home"))
  (check-catch 'value-error (run '(cd "/tmp") :cwd "/home"))

  ;; :env
  (check (run '(sh "-c" "test $FOO = bar") :env '(("FOO" . "bar"))) => 0)

  ;; list form with symbol head
  (check (run '(printf "%s" "hello world")) => 0)
) ;when

(when (os-windows?)
  (check (zero? (run "python3 -c pass")) => #t)
  (check (zero? (run "python3 -c 1/0")) => #f)
  (check (run "python3 -c print('hello')") => 0)

  (let ((orig-dir (getcwd)))
    (run "python3 -c pass" :cwd (os-temp-dir))
    (check (getcwd) => orig-dir)
  ) ;let

  (check-catch 'value-error (run "cd /tmp" :cwd (os-temp-dir)))
  (check-catch 'value-error (run '(cd "/tmp") :cwd (os-temp-dir)))

  ;; :env (need to preserve PATH on Windows)
  (let ((path-env (getenv "PATH")))
    (check (run "python3 -c pass" :env `(("FOO" . "bar") ("PATH" . ,path-env))) => 0))

  ;; list form with symbol head
  (run-set! 'pyprint "python3")
  (check (run '(pyprint "-c" "print('hello world')")) => 0)
) ;when

(check-report)
