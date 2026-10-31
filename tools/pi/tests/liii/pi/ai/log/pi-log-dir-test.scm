(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-dir
;; 返回 pi 异常请求日志的目录。
;;
;; 语法
;; ----
;; (pi-log-dir)
;;
;; 返回值
;; ----
;; path?
;; 固定为 ~/.gf/agent/logs（路径已展开用户主目录）。
;;
;; 注意
;; ----
;; 不受 GF_CONFIG_DIR 等配置目录环境变量影响：日志始终落在 ~/.gf/agent/logs。
;; 目录由 pi-log-init! 负责创建。

;; 期望的默认目录：用 path 拼接而不是字符串比较，避免 Windows 盘符与反斜杠差异

(define *default-dir* (path-join (path-expanduser (path "~/.gf/agent")) "logs"))

(define (same-path? a b)
  (path-equals? (path a) (path b))
) ;define

;; 1. 默认日志目录是 ~/.gf/agent/logs
(check-true (same-path? (pi-log-dir) *default-dir*))

;; 2. 环境变量不影响日志目录（不是 ~/.pi/agent 也不是 GF_CONFIG_DIR）

(define *orig-gf-config-dir* (getenv "GF_CONFIG_DIR" #f))

(define *env-tmp* (make-temp-dir "gf-pi-cfg-" ""))
(putenv "GF_CONFIG_DIR" *env-tmp*)
(check-true (same-path? (pi-log-dir) *default-dir*))
(if *orig-gf-config-dir*
  (putenv "GF_CONFIG_DIR" *orig-gf-config-dir*)
  (unsetenv "GF_CONFIG_DIR")
) ;if
(path-rmdir *env-tmp*)

;; 3. 显式初始化目录优先（供测试隔离使用）

(define *tmp* (make-temp-dir "gf-pi-log-dir-" ""))
(pi-log-init! *tmp*)
(check-true (same-path? (path-parent (pi-log-file-path)) *tmp*))
(pi-log-reset!)
(path-rmdir *tmp*)

;; 4. 复位后回到默认目录
(check-true (same-path? (pi-log-dir) *default-dir*))

(check-report)
