(import (liii check)
  (liii string)
  (liii path)
  (liii tempfile)
  (liii date-time)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-file-path
;; 返回当前日期对应的日志文件路径。
;;
;; 语法
;; ----
;; (pi-log-file-path)
;;
;; 返回值
;; ----
;; path?
;; 形如 ~/.gf/agent/logs/pi-YYYY-MM-DD.log；调用 pi-log-init! 指定目录后跟随该目录。
;;
;; 注意
;; ----
;; 按日期分文件，跨天自动落到新文件；只返回路径，不创建文件。

;; 1. 默认路径位于 ~/.gf/agent/logs 下（路径比较，兼容 Windows 盘符与反斜杠）
(check-true (path-equals? (path-parent (pi-log-file-path)) (pi-log-dir)))

;; 2. 文件名形如 pi-YYYY-MM-DD.log

(define today (date->iso-8601 (timestamp-date (current-system-timestamp))))

(check (string-ends? (path->string (pi-log-file-path))
         (string-append "pi-" today ".log")
       ) ;string-ends?
  =>
  #t
) ;check

;; 3. 返回值是绝对路径（不用 "/" 前缀判断，Windows 下以盘符开头）
(check-true (path-absolute? (pi-log-file-path)))

;; 4. 显式初始化目录后，文件路径跟随初始化目录

(define *base* (make-temp-dir "gf-pi-log-path-" ""))

(define *dir* (path-join (path *base*) "logs"))
(pi-log-init! *dir*)
(check-true (path-equals? (path-parent (pi-log-file-path)) (path *dir*)))
(pi-log-reset!)
(path-rmdir *dir*)
(path-rmdir *base*)

(check-report)
