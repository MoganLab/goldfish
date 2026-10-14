(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; run-streaming
;; 以流式回调方式执行命令并逐行处理子进程输出。
;;
;; 语法
;; ----
;; (run-streaming command)
;; (run-streaming command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;; 待执行的命令。
;;
;; keyword value ... : 可选
;; :on-stdout — 标准输出每行回调过程 (lambda (line) ...)。
;; :on-stderr — 标准错误每行回调过程 (lambda (line) ...)。
;; :cwd — 工作目录。
;; :env — 环境变量键值对列表。
;; :timeout — 超时时间（秒）。
;;
;; 返回值
;; ----
;; integer
;; 子进程退出码，0 表示正常退出。

(let* ((lines '())
       (child-code (string-append "import sys\n" "print('alpha', flush=True)\n"
                     "print('beta', flush=True)\n"
                     "print('gamma', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (exit-code
         (run-streaming `(,"python3" ,"-c" ,child-code)
           :on-stdout (lambda (line) (set! lines (cons line lines)))
         ) ;run-streaming
       ) ;exit-code
      ) ;
  (check exit-code => 0)
  (check (reverse lines) => '("alpha" "beta" "gamma"))
) ;let*

;; 错误参数检查
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :on-stdout "not-a-proc")
) ;check-catch
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :on-stderr "not-a-proc")
) ;check-catch
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :timeout "not-a-num")
) ;check-catch

(check-report)
