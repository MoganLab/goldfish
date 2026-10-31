(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-init!
;;
;; 幂等初始化：确定日志目录并创建出来（含多级父目录），重复调用不报错。

(define *base* (make-temp-dir "gf-pi-log-init-" ""))

(define *dir* (path-join (path *base*) "nested/logs"))

;; 1. 初始化前日志目录不存在
(check (path-exists? *dir*) => #f)

;; 2. 初始化创建日志目录（含多级父目录）
(pi-log-init! *dir*)
(check (path-exists? *dir*) => #t)

;; 3. 重复初始化幂等：不抛异常，日志文件仍落在该目录
(pi-log-init! *dir*)
(check (path->string (path-parent (pi-log-file-path))) => (path->string *dir*))

;; 4. 默认（不传目录）时使用 ~/.gf/agent/logs
(pi-log-reset!)
(pi-log-init!)
(check (string-ends? (path->string (pi-log-file-path)) "/.gf/agent/logs") => #f)
(check (string-ends? (path->string (pi-log-dir)) "/.gf/agent/logs") => #t)

(pi-log-reset!)
(path-rmdir *dir*)
(path-rmdir (path-join (path *base*) "nested"))
(path-rmdir *base*)

(check-report)
