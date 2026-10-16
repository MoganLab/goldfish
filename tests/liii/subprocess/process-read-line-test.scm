(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-read-line
;; 从子进程的标准输出或标准错误读取一行文本。
;;
;; 语法
;; ----
;; (process-read-line proc)
;; (process-read-line proc keyword value ...)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; keyword value ... : 可选
;; :stream — 读取的数据流，'stdout（默认）或 'stderr。
;;
;; 返回值
;; ----
;; string 或 eof-object
;; 包含单行内容的字符串（不含末尾换行符）；当到达文件末尾时返回 EOF 对象。

(let* ((child-code (string-append "import sys\n"
                     "print('line 1', flush=True)\n"
                     "print('line 2', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe)
       ) ;proc
      ) ;
  (check (process-read-line proc :stream 'stdout) => "line 1")
  (check (process-read-line proc :stream 'stdout) => "line 2")
  (check (eof-object? (process-read-line proc :stream 'stdout)) => #t)
  (process-close proc)
  (check (process-wait proc 2000) => 0)
) ;let*

;; 错误处理
(check-catch 'type-error (process-read-line "invalid"))
(let ((proc (process-spawn '("python3" "-c" "pass") :stdout 'pipe)))
  (check-catch 'value-error (process-read-line proc :stream 'stdin))
  (check-catch 'type-error (process-read-line proc :stream 123))
  (process-close proc)
  (process-wait proc 2000)
) ;let

(check-report)
