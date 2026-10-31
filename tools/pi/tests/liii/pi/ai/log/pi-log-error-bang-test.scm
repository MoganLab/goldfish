(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii subprocess)
  (liii tempfile)
  (liii sys)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-error!
;;
;; 以 ERROR 级别写一条日志到 ~/.gf/agent/logs（测试重定向到临时目录），
;; 并且写入后立即 flush 并关闭文件端口：进程退出码不受日志影响。

(define *base* (make-temp-dir "gf-pi-log-error-" ""))

(define *dir* (path-join (path *base*) "logs"))

(define *script* (path-join (path *base*) "exit-script.scm"))

(pi-log-init! *dir*)

(define *log-file* (pi-log-file-path))

;; 1. 写入内容与级别标记
(pi-log-error! "first entry")
(check-true (string-contains? (path-read-text *log-file*) "first entry"))
(check-true (string-contains? (path-read-text *log-file*) "[ERROR]"))

;; 2. 多次写入追加到同一文件
(pi-log-error! "second entry")
(check-true (string-contains? (path-read-text *log-file*) "second entry"))

(pi-log-reset!)

;; 3. 关闭开关（PI_LOG=off）时不写入
(putenv "PI_LOG" "off")
(pi-log-init! *dir*)
(pi-log-error! "should not be written")
(check (string-contains? (path-read-text *log-file*) "should not be written")
  =>
  #f
) ;check
(unsetenv "PI_LOG")
(pi-log-reset!)

;; 4. 写日志不改变进程退出码（s7 exit-hook 有分配会吞退出码，见 issue #1399）

(define (run-exit-script body code)
  (path-write-text *script*
    (string-append "(set! *load-path* (append (list \"tools/pi\") *load-path*))\n"
      "(import (liii pi ai log))\n"
      "(pi-log-init! \""
      *base*
      "\")\n"
      body
      "(exit "
      (number->string code)
      ")\n"
    ) ;string-append
  ) ;path-write-text
  (process-wait (process-spawn (list (executable) (path->string *script*)) :stdout 'pipe)
  ) ;process-wait
) ;define

(pi-log-init! *dir*)
(check (run-exit-script "(pi-log-error! \"boom\")\n" 1) => 1)
(check (run-exit-script "(pi-log-error! \"a\")\n(pi-log-error! \"b\")\n" 3)
  =>
  3
) ;check
(check (run-exit-script "(pi-log-error! \"done\")\n" 0) => 0)

(pi-log-reset!)
(path-unlink *log-file*)
(path-unlink *script*)
(path-rmdir *dir*)
(path-rmdir *base*)

(check-report)
