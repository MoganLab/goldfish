(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-close-stdin
;; 关闭子进程的标准输入管道（向子进程传递 EOF）。
;;
;; 语法
;; ----
;; (process-close-stdin proc)
;;
;; 参数
;; ----
;; proc : process
;; 具有标准输入管道的进程对象。
;;
;; 返回值
;; ----
;; 未定义

(let* ((child-code (string-append "import sys\n"
                     "data = sys.stdin.read()\n"
                     "print('len:' + str(len(data)), flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdin 'pipe :stdout 'pipe)
       ) ;proc
      ) ;
  (process-write proc "test1234")
  (process-close-stdin proc)
  (check (process-read-line proc :stream 'stdout) => "len:8")
  (process-close proc)
  (check (process-wait proc 2000) => 0)
) ;let*

;; 类型错误
(check-catch 'type-error (process-close-stdin "not-a-proc"))
(check-catch 'type-error (process-close-stdin '()))

(check-report)
