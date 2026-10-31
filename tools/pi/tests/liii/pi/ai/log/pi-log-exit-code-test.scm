(import (liii check)
        (liii string)
        (liii os)
        (liii path)
        (liii subprocess)
        (liii tempfile)
        (liii sys)
        (liii pi ai log))

(check-set-mode! 'report-failed)

;; 写日志不应改变进程退出码
;;
;; s7 的 (exit N) 在应用 *exit-hook* 之后才 s7_quit 并读取退出码（issue #1399），
;; 钩子里只要有分配就可能把退出码吞成 0。本模块写日志后立即 flush 并关闭文件端口，
;; 让退出钩子成为空操作；这里用子进程验证退出码确实保持不变。

(define *base* (make-temp-dir "gf-pi-log-exit-" ""))
(define *script* (path-join (path *base*) "exit-script.scm"))

(define (run-exit-script body code)
  (path-write-text *script*
    (string-append "(set! *load-path* (append (list \"tools/pi\") *load-path*))\n"
      "(import (liii pi ai log))\n"
      "(pi-log-init! \"" *base* "\")\n"
      body
      "(exit " (number->string code) ")\n"
    ) ;string-append
  ) ;path-write-text
  (let* ((child (process-spawn (list (executable) (path->string *script*)) :stdout 'pipe)
         ) ;child
         (code-out (process-wait child))
        ) ;
    code-out
  ) ;let*
) ;define

;; 1. 只初始化不写日志：退出码保持
(check (run-exit-script "" 1) => 1)

;; 2. 写一条 ERROR 日志后退出：退出码保持
(check (run-exit-script "(pi-log-error! \"boom\")\n" 1) => 1)

;; 3. 写异常 HTTP 日志后退出：退出码保持
(check (run-exit-script
         "(pi-log-http-error! 429 \"{\\\"error\\\":\\\"x\\\"}\" '())\n"
         1
       ) ;run-exit-script
  => 1
) ;check

;; 4. 多次写日志后退出：退出码保持
(check (run-exit-script
         "(pi-log-error! \"a\")\n(pi-log-error! \"b\")\n(pi-log-error! \"c\")\n"
         3
       ) ;run-exit-script
  => 3
) ;check

;; 5. 写日志后以 0 退出：退出码保持
(check (run-exit-script "(pi-log-error! \"done\")\n" 0) => 0)

(pi-log-reset!)
(path-unlink *script*)
(path-rmdir *base*)

(check-report)
