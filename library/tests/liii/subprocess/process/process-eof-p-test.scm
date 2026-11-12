;; process-eof?
;; 判定子进程指定输出流是否已到达末尾。
;;
;; 语法
;; ----
;; (process-eof? proc)
;; (process-eof? proc keyword value ...)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; keyword value ... : 可选
;; :stream — 查询的数据流，'stdout（默认）或 'stderr。
;;
;; 返回值
;; ----
;; boolean
;; 流已读到末尾返回 #t，否则返回 #f。

(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; 读完全部输出后为 #t
(let* ((child-code (string-append "import sys\n"
                     "print('line', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc (process-spawn `("python3" "-c" ,child-code) :stdout 'pipe))
      ) ;
  (check (process-eof? proc) => #f)
  (check (process-read-line proc :stream 'stdout) => "line")
  (check (eof-object? (process-read-line proc :stream 'stdout)) => #t)
  (check (process-eof? proc) => #t)
  (process-close proc)
  (process-wait proc 2000)
) ;let*

;; :stream 'stderr
(let* ((child-code (string-append "import sys\n"
                     "print('err', file=sys.stderr, flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc (process-spawn `("python3" "-c" ,child-code)
               :stdout 'pipe
               :stderr 'pipe
             )) ;process-spawn
      ) ;
  (check (process-eof? proc :stream 'stderr) => #f)
  (check (process-read-line proc :stream 'stderr) => "err")
  (check (eof-object? (process-read-line proc :stream 'stderr)) => #t)
  (check (process-eof? proc :stream 'stderr) => #t)
  (process-close proc)
  (process-wait proc 2000)
) ;let*

;; 进程结束后未读取的流不会立即 EOF，读空后为 #t
(let ((proc (process-spawn '("python3" "-c" "pass") :stdout 'pipe)))
  (process-wait proc 2000)
  (check (eof-object? (process-read-line proc :stream 'stdout)) => #t)
  (check (process-eof? proc) => #t)
  (process-close proc)
) ;let

;; 错误处理
(check-catch 'type-error (process-eof? #f))
(check-catch 'type-error (process-eof? "invalid"))
(let ((proc (process-spawn '("python3" "-c" "pass") :stdout 'pipe)))
  (check-catch 'value-error (process-eof? proc :stream 'stdin))
  (check-catch 'type-error (process-eof? proc :stream 123))
  (process-close proc)
  (process-wait proc 2000)
) ;let

(check-report)
