(import (liii check) (liii string) (liii path) (liii tempfile) (liii pi ai log))

(check-set-mode! 'report-failed)

;; pi-log-reset!
;; 关闭日志文件端口并清空状态。
;;
;; 语法
;; ----
;; (pi-log-reset!)
;;
;; 返回值
;; ----
;; unspecified
;;
;; 注意
;; ----
;; 复位后回到默认目录 ~/.gf/agent/logs，可重新用 pi-log-init! 指向别的目录。
;; 测试用它隔离用例，避免把日志写到真实目录。

(define *base* (make-temp-dir "gf-pi-log-reset-" ""))

(define *dir-a* (path-join (path *base*) "logs-a"))

(define *dir-b* (path-join (path *base*) "logs-b"))

;; 1. 复位后回到默认目录 ~/.gf/agent/logs
(pi-log-init! *dir-a*)
(check (path->string (path-parent (pi-log-file-path)))
  =>
  (path->string *dir-a*)
) ;check
(pi-log-reset!)
(check (string-ends? (path->string (pi-log-dir)) "/.gf/agent/logs") => #t)

;; 2. 复位后可重新初始化到另一个目录
(pi-log-init! *dir-b*)
(check (path->string (path-parent (pi-log-file-path)))
  =>
  (path->string *dir-b*)
) ;check

;; 3. 复位后写入不再落到原目录
(pi-log-reset!)
(check (path-exists? *dir-a*) => #t)

(pi-log-reset!)
(path-rmdir *dir-a*)
(path-rmdir *dir-b*)
(path-rmdir *base*)

(check-report)
