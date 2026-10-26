(import (liii check) (liii subprocess) (liii string))

(check-set-mode! 'report-failed)

;; run-streaming-chunks
;; 以块级流式回调方式执行命令，stdout/stderr 合并到同一管道，
;; 按原始字节块回调，保留两个流的写入顺序。
;;
;; 语法
;; ----
;; (run-streaming-chunks command)
;; (run-streaming-chunks command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;; 待执行的命令。
;;
;; keyword value ... : 可选
;; :on-chunk — 输出字节块回调过程 (lambda (chunk) ...)。
;; :cwd — 工作目录。
;; :env — 环境变量键值对列表。
;; :timeout — 超时时间（秒）。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 子进程退出码，0 表示正常退出；超时强杀时返回 #f。

;; 基本用法：收集全部输出块
(let* ((chunks '())
       (code
         (run-streaming-chunks '("python3" "-c" "print('hello')")
           :on-chunk (lambda (c) (set! chunks (cons c chunks)))
         ) ;run-streaming-chunks
       ) ;code
      ) ;
  (check code => 0)
  (check (string-join (reverse chunks) "") => "hello\n")
) ;let*

;; stdout/stderr 合并后保留写入顺序
(let* ((chunks '())
       (child-code (string-append "import sys\n"
                     "print('out-1', flush=True)\n"
                     "sys.stderr.write('err-1\\n')\n"
                     "sys.stderr.flush()\n"
                     "print('out-2', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (code
         (run-streaming-chunks `("python3" "-c" ,child-code)
           :on-chunk (lambda (c) (set! chunks (cons c chunks)))
         ) ;run-streaming-chunks
       ) ;code
       (text (string-join (reverse chunks) ""))
      ) ;
  (check code => 0)
  (check text => "out-1\nerr-1\nout-2\n")
) ;let*

;; 无回调时仅等待退出码
(let ((code (run-streaming-chunks '("python3" "-c" "print('ignored')"))))
  (check code => 0)
) ;let

;; 非零退出码透传
(let ((code (run-streaming-chunks '("python3" "-c" "import sys; sys.exit(3)"))))
  (check code => 3)
) ;let

;; 超时强杀并返回 #f，已产生的输出仍回调
(let* ((chunks '())
       (ret
         (run-streaming-chunks
           '("python3" "-c" "import time; print('first', flush=True); time.sleep(10)")
           :on-chunk (lambda (c) (set! chunks (cons c chunks)))
           :timeout  0.5
         ) ;run-streaming-chunks
       ) ;ret
      ) ;
  (check ret => #f)
  (check (string-contains (string-join (reverse chunks) "") "first") => #t)
) ;let*

;; 错误参数检查
(check-catch 'type-error
  (run-streaming-chunks '("python3" "-c" "pass") :on-chunk "not-a-procedure")
) ;check-catch

(check-catch 'type-error
  (run-streaming-chunks '("python3" "-c" "pass") :timeout "not-a-number")
) ;check-catch

(check-catch 'value-error
  (run-streaming-chunks '("python3" "-c" "pass") :timeout -1)
) ;check-catch

(check-report)
