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
;; 字符串形式通过 /bin/sh -c 执行，列表形式逐字传递参数。
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
;; 1. 字符串命令支持 shell 特性（通配符、变量展开等）。
;; 2. 列表形式更安全，不经过 shell 解析。
;; 3. 命令中显式包含 cd 时不可同时设置 :cwd，否则抛 value-error。

(when (os-linux?)
  (check (zero? (run "true")) => #t)
  (check (zero? (run "false")) => #f)
  (check (run "echo hello") => 0)
  (check (zero? (run '("true"))) => #t)
  (check (zero? (run '("false"))) => #f)

  (let ((orig-dir (getcwd)))
    (run "pwd" :cwd "/tmp")
    (check (getcwd) => orig-dir)
  ) ;let

  (let ((orig-dir (getcwd)))
    (run '("pwd") :cwd "/tmp")
    (check (getcwd) => orig-dir)
  ) ;let

  (check-catch 'value-error (run "cd /tmp" :cwd "/home"))
  (check-catch 'value-error (run '("cd" "/tmp") :cwd "/home"))
  (check-catch 'value-error (run '(cd "/tmp") :cwd "/home"))

  ;; :env
  (check (run "test $FOO = bar" :env '(("FOO" . "bar"))) => 0)

  ;; list form bypasses shell
  (check (run '("printf" "%s" "hello world")) => 0)
) ;when

(check-report)
