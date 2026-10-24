(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-write
;; 向子进程的标准输入写入字符串。
;;
;; 语法
;; ----
;; (process-write proc str)
;;
;; 参数
;; ----
;; proc : process
;; 具有可写标准输入管道的进程对象。
;;
;; str : string
;; 要写入的内容字符串。
;;
;; 返回值
;; ----
;; 未定义

(let* ((child-code (string-append "import sys\n"
                     "line = sys.stdin.readline().strip()\n"
                     "print('got:' + line, flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdin 'pipe :stdout 'pipe)
       ) ;proc
      ) ;
  (process-write proc "hello\n")
  (check (process-read-line proc :stream 'stdout) => "got:hello")
  (process-close-stdin proc)
  (process-close proc)
  (process-wait proc 2000)
) ;let*

;; 类型错误
(check-catch 'type-error (process-write "not-a-proc" "data"))
(let ((proc (process-spawn '("python3" "-c" "pass") :stdin 'pipe)))
  (check-catch 'type-error (process-write proc 123))
  (process-close-stdin proc)
  (process-close proc)
  (process-wait proc 2000)
) ;let

(check-report)
