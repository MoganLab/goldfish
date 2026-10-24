(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-last-activity-time
;; 返回子进程最后一次产生 stdout/stderr 输出或初始启动时的单调时钟时间戳（秒）。
;;
;; 语法
;; ----
;; (process-last-activity-time proc)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; 返回值
;; ----
;; real
;; 单调时钟时间戳（秒，大于 0 的实数）。

(let* ((child-code (string-append "import sys, time\n" "time.sleep(0.2)\n"
                     "print('step1', flush=True)\n" "time.sleep(0.2)\n"
                     "print('step2', flush=True)\n"
                   ) ;string-append
       ) ;child-code
       (proc
         (process-spawn `(,"python3" ,"-c" ,child-code) :stdout 'pipe)
       ) ;proc
       (t0 (process-last-activity-time proc))
      ) ;
  (check (real? t0) => #t)
  (check (> t0 0) => #t)
  (let ((l1 (process-read-line proc :stream 'stdout)))
    (check l1 => "step1")
    (let ((t1 (process-last-activity-time proc)))
      (check (real? t1) => #t)
      (check (>= t1 t0) => #t)
      (let ((l2 (process-read-line proc :stream 'stdout)))
        (check l2 => "step2")
        (let ((t2 (process-last-activity-time proc)))
          (check (real? t2) => #t)
          (check (>= t2 t1) => #t)
          (check (process-wait proc 2000) => 0)
          (process-close proc)
        ) ;let
      ) ;let
    ) ;let
  ) ;let
) ;let*

;; 错误处理
(check-catch 'type-error (process-last-activity-time "not-a-proc"))
(check-catch 'type-error (process-last-activity-time 123))
(check-catch 'type-error (process-last-activity-time '()))

(check-report)
