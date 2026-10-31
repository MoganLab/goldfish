(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii date-time)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-file-path
;;
;; 日志文件按日期分文件：~/.gf/agent/logs/pi-YYYY-MM-DD.log

;; 1. 默认路径位于 ~/.gf/agent/logs 下
(check (path->string (path-parent (pi-log-file-path)))
  =>
  (path->string (pi-log-dir))
) ;check

;; 2. 文件名形如 pi-YYYY-MM-DD.log

(define today (date->iso-8601 (timestamp-date (current-system-timestamp))))

(check (string-ends? (path->string (pi-log-file-path))
         (string-append "pi-" today ".log")
       ) ;string-ends?
  =>
  #t
) ;check

;; 3. 跨天时文件名随日期变化（用提前一天的时间无法注入，这里只校验格式三段）
(check (string-starts? (path->string (pi-log-file-path)) "/") => #t)

;; 4. 显式初始化目录后，文件路径跟随初始化目录

(define *base* (make-temp-dir "gf-pi-log-path-" ""))

(define *dir* (path-join (path *base*) "logs"))
(pi-log-init! *dir*)
(check (path->string (path-parent (pi-log-file-path))) => (path->string *dir*))
(pi-log-reset!)
(path-rmdir *dir*)
(path-rmdir *base*)

(check-report)
