;; process-poll-io
;; 阻塞式轮询子进程的 I/O 事件。
;;
;; 语法
;; ----
;; (process-poll-io proc)
;; (process-poll-io proc timeout-ms)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; timeout-ms : integer（可选）
;; 等待 I/O 事件的超时毫秒数，默认 50。
;;
;; 返回值
;; ----
;; symbol 或 #f
;; 'stdout — stdout 有数据可读；'stderr — stderr 有数据可读；
;; 'both — 两个流均可读；'eof — 两个流均已到达末尾；
;; 超时无事件时返回 #f。

(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; stdout 可读事件
(let* ((child-code (string-append "import sys\n"
                     "print('hello', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc (process-spawn `("python3" "-c" ,child-code) :stdout 'pipe))
       (ev (process-poll-io proc 2000))
      ) ;
  (check (and (memq ev '(stdout both)) #t) => #t)
  (check (process-read-line proc :stream 'stdout) => "hello")
  (process-close proc)
  (process-wait proc 2000)
) ;let*

;; 无事件时超时返回 #f
(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(0.3)")
             :stdout 'pipe
           ) ;process-spawn
      ) ;proc
     ) ;
  (check (process-poll-io proc 10) => #f)
  (process-close proc)
  (process-wait proc 2000)
) ;let

;; stderr 可读事件
(let* ((child-code (string-append "import sys\n"
                     "print('oops', file=sys.stderr, flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc (process-spawn `("python3" "-c" ,child-code)
               :stdout 'pipe
               :stderr 'pipe
             ) ;process-spawn
       ) ;
       (ev (process-poll-io proc 2000))
      ) ;
  (check (and (memq ev '(stderr both)) #t) => #t)
  (check (process-read-line proc :stream 'stderr) => "oops")
  (process-close proc)
  (process-wait proc 2000)
) ;let*

;; 两流全部读完后返回 'eof
(let* ((proc (process-spawn '("python3" "-c" "print('x', flush=True)")
             :stdout 'pipe
           ) ;process-spawn
       ) ;
      ) ;
  (process-wait proc 2000)
  (check (process-read-line proc :stream 'stdout) => "x")
  (check (process-poll-io proc 100) => 'eof)
  (process-close proc)
) ;let*

;; 默认超时参数（单参数形式）
(let ((proc (process-spawn '("python3" "-c" "pass") :stdout 'pipe)))
  (process-poll-io proc)
  (process-close proc)
  (process-wait proc 2000)
) ;let

;; 错误处理
(check-catch 'type-error (process-poll-io #f))
(check-catch 'type-error (process-poll-io "invalid"))
(let ((proc (process-spawn '("python3" "-c" "pass") :stdout 'pipe)))
  (check-catch 'type-error (process-poll-io proc 1.5))
  (check-catch 'value-error (process-poll-io proc -1))
  (process-close proc)
  (process-wait proc 2000)
) ;let

(check-report)
