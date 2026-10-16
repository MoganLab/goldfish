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
;; :idle-timeout — 空闲超时时间（秒，无输出超过设定时间强杀）。
;; :on-idle — 空闲预警回调过程 (lambda (proc idle-secs) ...)。
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

;; 超时强杀并返回 #f
(let ((ret (run-streaming '("python3" "-c" "import time; time.sleep(5)")
             :on-stdout (lambda (line) #f)
             :timeout   0.2
           ) ;run-streaming
      ) ;ret
     ) ;
  (check ret => #f)
) ;let

;; 双流 (stdout + stderr) 流式处理
(let* ((stdout-lines '())
       (stderr-lines '())
       (child-code (string-append "import sys\n" "print('out-1', flush=True)\n"
                     "sys.stderr.write('err-1\\n')\n" "sys.stderr.flush()\n"
                     "print('out-2', flush=True)\n"
                     "sys.stderr.write('err-2\\n')\n" "sys.stderr.flush()\n"
                   ) ;string-append
       ) ;child-code
       (code
         (run-streaming `(,"python3" ,"-c" ,child-code)
           :on-stdout (lambda (l) (set! stdout-lines (cons l stdout-lines)))
           :on-stderr (lambda (l) (set! stderr-lines (cons l stderr-lines)))
         ) ;run-streaming
       ) ;code
      ) ;
  (check code => 0)
  (check (reverse stdout-lines) => '("out-1" "out-2"))
  (check (reverse stderr-lines) => '("err-1" "err-2"))
) ;let*

;; 周期性产生输出的长任务不被空闲超时误杀
(let* ((lines '())
       (child-code (string-append "import sys, time\n" "for i in range(3):\n"
                     "    time.sleep(0.2)\n"
                     "    print(f'beat-{i}', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (ret
         (run-streaming `(,"python3" ,"-c" ,child-code)
           :on-stdout    (lambda (l) (set! lines (cons l lines)))
           :timeout      10
           :idle-timeout 0.6
         ) ;run-streaming
       ) ;ret
      ) ;
  (check ret => 0)
  (check (reverse lines) => '("beat-0" "beat-1" "beat-2"))
) ;let*

;; 静默挂起任务触发空闲超时强杀并返回 #f
(let ((ret (run-streaming '("python3" "-c" "import time; time.sleep(10)")
             :on-stdout    (lambda (l) #f)
             :timeout      30
             :idle-timeout 0.3
           ) ;run-streaming
      ) ;ret
     ) ;
  (check ret => #f)
) ;let

;; 空闲预警回调 :on-idle 触发测试
(let* ((idle-calls '())
       (child-code (string-append "import sys, time\n"
                     "time.sleep(0.2)\n"
                     "print('ready', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (ret
         (run-streaming `(,"python3" ,"-c" ,child-code)
           :on-idle
           (lambda (proc idle-secs) (set! idle-calls (cons idle-secs idle-calls)))
           :idle-timeout 2.0
         ) ;run-streaming
       ) ;ret
      ) ;
  (check ret => 0)
  (check (> (length idle-calls) 0) => #t)
  (check (> (car idle-calls) 0) => #t)
) ;let*

;; 错误参数检查
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :on-stdout "not-a-proc")
) ;check-catch
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :on-stderr "not-a-proc")
) ;check-catch
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :on-idle "not-a-proc")
) ;check-catch
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :timeout "not-a-num")
) ;check-catch
(check-catch 'type-error
  (run-streaming '("python3" "-c" "pass") :idle-timeout "not-a-num")
) ;check-catch
(check-catch 'value-error (run-streaming '("python3" "-c" "pass") :timeout -1))
(check-catch 'value-error
  (run-streaming '("python3" "-c" "pass") :idle-timeout -1)
) ;check-catch
(check-catch 'value-error
  (run-streaming '("python3" "-c" "pass") :idle-timeout 0)
) ;check-catch
(check-catch 'value-error
  (run-streaming '("python3" "-c" "pass") :timeout 1 :idle-timeout 2)
) ;check-catch

(check-report)
