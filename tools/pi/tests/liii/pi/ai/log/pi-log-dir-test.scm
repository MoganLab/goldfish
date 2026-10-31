(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-dir
;;
;; 日志目录固定为 ~/.gf/agent/logs。

;; 1. 默认日志目录是 ~/.gf/agent/logs
(check (path->string (pi-log-dir))
  =>
  (path->string (path-join (path-expanduser (path "~/.gf/agent")) "logs"))
) ;check

(check (string-ends? (path->string (pi-log-dir)) "/.gf/agent/logs") => #t)

;; 2. 环境变量不影响日志目录（不是 ~/.pi/agent 也不是 GF_CONFIG_DIR）

(define *orig-gf-config-dir* (getenv "GF_CONFIG_DIR" #f))
(putenv "GF_CONFIG_DIR" "/tmp/gf-pi-cfg-a")
(check (string-ends? (path->string (pi-log-dir)) "/.gf/agent/logs") => #t)
(if *orig-gf-config-dir*
  (putenv "GF_CONFIG_DIR" *orig-gf-config-dir*)
  (unsetenv "GF_CONFIG_DIR")
) ;if

;; 3. 显式初始化目录优先（供测试隔离使用）

(define *tmp* (make-temp-dir "gf-pi-log-dir-" ""))
(pi-log-init! *tmp*)
(check (path->string (path-parent (pi-log-file-path))) => *tmp*)
(pi-log-reset!)
(path-rmdir *tmp*)

;; 4. 复位后回到默认目录
(check (string-ends? (path->string (pi-log-dir)) "/.gf/agent/logs") => #t)

(check-report)
