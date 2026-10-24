(import (liii check) (liii subprocess) (liii time))

(check-set-mode! 'report-failed)

;; process-idle-seconds
;; 返回自子进程上次产生 stdout/stderr 输出（或初始启动）以来经过的空闲秒数。
;;
;; 语法
;; ----
;; (process-idle-seconds proc)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; 返回值
;; ----
;; real
;; 自上次活动以来的空闲秒数（大于或等于 0 的非负实数）。

(let* ((child-code (string-append "import sys, time\n" "time.sleep(0.3)\n"
                     "print('hello', flush=True)\n" "time.sleep(0.3)\n"
                     "print('world', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe)
       ) ;proc
      ) ;
  ;; 输出间隔期间，空闲时间单调递增
  (sleep 0.1)
  (let ((s1 (process-idle-seconds proc)))
    (check (real? s1) => #t)
    (check (> s1 0) => #t)
    (sleep 0.1)
    (let ((s2 (process-idle-seconds proc)))
      (check (> s2 s1) => #t)
      ;; 读取到新数据后，空闲秒数立即归零或重置为极小值（< 0.05 秒）
      (let ((line1 (process-read-line proc :stream 'stdout)))
        (check line1 => "hello")
        (let ((reset-idle (process-idle-seconds proc)))
          (check (< reset-idle 0.05) => #t)
          (let ((line2 (process-read-line proc :stream 'stdout)))
            (check line2 => "world")
            (check (process-wait proc 2000) => 0)
            (process-close proc)
          ) ;let
        ) ;let
      ) ;let
    ) ;let
  ) ;let
) ;let*

;; 错误处理
(check-catch 'type-error (process-idle-seconds "not-a-proc"))
(check-catch 'type-error (process-idle-seconds 123))
(check-catch 'type-error (process-idle-seconds '()))

(check-report)
