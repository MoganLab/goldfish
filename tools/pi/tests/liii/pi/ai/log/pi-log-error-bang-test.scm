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
;; 以 ERROR 级别写一条日志。
;;
;; 语法
;; ----
;; (pi-log-error! message)
;;
;; 参数
;; ----
;; message : string?
;; 日志正文，可以包含换行（用于把响应体原文成段写出）。
;;
;; 返回值
;; ----
;; boolean
;; 写入成功返回 #t；日志开关关闭或写盘失败返回 #f。
;;
;; 注意
;; ----
;; 每写一条就 flush 并关闭文件端口：s7 的 exit 在应用退出钩子之后才读退出码，
;; 钩子里有分配会把退出码吞成 0（goldfish issue 1399），关端口后钩子成为空操作。
;; 写盘失败一律静默，不影响请求主流程。

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
