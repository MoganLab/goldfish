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
;; 幂等初始化：创建日志目录（含多级父目录）、按日期命名日志文件，
;; 重复初始化不重复打开文件端口。

(define *base* (make-temp-dir "gf-pi-log-init-" ""))

(define *dir* (path-join (path *base*) "nested/logs"))

(define *log-file* #f)

;; 1. 初始化前日志目录不存在
(check (path-exists? *dir*) => #f)

;; 2. 初始化创建日志目录（含多级父目录）
(pi-log-init! *dir*)
(check (path-exists? *dir*) => #t)

;; 3. 写入后日志文件出现在该目录下
(pi-log-error! "first entry")
(set! *log-file* (pi-log-file-path))
(check (path-exists? *log-file*) => #t)
(check-true (string-contains? (path-read-text *log-file*) "first entry"))

;; 4. 重复初始化幂等：不抛异常，后续写入落到同一文件
(pi-log-init! *dir*)
(pi-log-error! "second entry")
(check-true (string-contains? (path-read-text (pi-log-file-path)) "second entry")
) ;check-true

;; 5. 每次写入都带级别标记
(check-true (string-contains? (path-read-text (pi-log-file-path)) "[ERROR]"))

;; 6. PI_LOG=off 时关闭文件日志，pi-log-enabled? 为假
(putenv "PI_LOG" "off")
(check (pi-log-enabled?) => #f)
(putenv "PI_LOG" "on")
(check (pi-log-enabled?) => #t)
(unsetenv "PI_LOG")

(pi-log-reset!)
(path-unlink *log-file*)
(path-rmdir *dir*)
(path-rmdir (path-join (path *base*) "nested"))
(path-rmdir *base*)

(check-report)
