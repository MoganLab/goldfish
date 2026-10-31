(import (liii check) (liii path) (liii tempfile) (liii pi ai log))

(check-set-mode! 'report-failed)

;; pi-log-init!
;; 初始化日志目录（幂等）。
;;
;; 语法
;; ----
;; (pi-log-init!)
;; (pi-log-init! dir)
;;
;; 参数
;; ----
;; dir : path? 或 string?
;; 可选，覆盖默认日志目录（测试用于把日志写到临时目录）。
;;
;; 返回值
;; ----
;; boolean
;; 日志目录可用且日志开关开启时返回 #t，否则 #f。
;;
;; 注意
;; ----
;; 会递归创建目录（含多级父目录）；重复调用不报错、不改变已确定的目录。

;; 用系统临时目录派生路径，避免在 Windows 上把 /tmp 解析成当前盘符下的绝对路径

(define *base* (make-temp-dir "gf-pi-log-init-" ""))

(define *dir* (path-join (path *base*) "nested/logs"))

(define *default-dir* (path-join (path-expanduser (path "~/.gf/agent")) "logs"))

;; 1. 初始化前日志目录不存在
(check (path-exists? *dir*) => #f)

;; 2. 初始化创建日志目录（含多级父目录），并返回可用状态
(check-true (and (pi-log-init! *dir*) #t))
(check (path-exists? *dir*) => #t)

;; 3. 重复初始化幂等：不抛异常，日志文件仍落在该目录
(pi-log-init! *dir*)
(check-true (path-equals? (path-parent (pi-log-file-path)) (path *dir*)))

;; 4. 不传目录时使用默认目录 ~/.gf/agent/logs
(pi-log-reset!)
(pi-log-init!)
(check-true (path-equals? (pi-log-dir) *default-dir*))
(check-true (path-equals? (path-parent (pi-log-file-path)) *default-dir*))

(pi-log-reset!)
(path-rmdir *dir*)
(path-rmdir (path-join (path *base*) "nested"))
(path-rmdir *base*)

(check-report)
