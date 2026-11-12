;; %bash-run-abortable
;; ! 命令的 ESC 中止执行路径：流式执行 + 50ms 轮询点探测。
;;
;; 语法
;; ----
;; (%bash-run-abortable command timeout check-abort)
;;
;; 参数
;; ----
;; command : string
;; 要执行的 bash 命令。
;; timeout : real
;; 超时秒数。
;; check-abort : procedure
;; 中止探测谓词，命中即杀进程树返回退出码 -2。
;;
;; 返回值
;; ----
;; (values 清理后的合并输出 退出码)：-1 超时，-2 中止。

(import (scheme base)
  (liii check)
  (liii go)
  (liii time)
  (scheme time)
  (liii string)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 探测恒为假：正常返回真实退出码，输出已清理
(let-values (((text code)
              (%bash-run-abortable "echo 'plain ok'" 30 (lambda () #f))
            ) ;
           ) ;
  (check code => 0)
  (check-true (string-contains? text "plain ok"))
) ;let-values

;; 2. echo 先完成再 sleep 30，0.5s 后信号到达：轮询点探测杀树返回 -2，
;; 已产生输出保留且总耗时不等自然退出
(let* ((sig (make-chan 1))
       (t (go-call (lambda (ch) (sleep 0.5) (chan-send! ch 'abort 0)) sig))
       (t0 (current-second))
      ) ;
  (let-values (((text code)
                (%bash-run-abortable "echo 'partial'; sleep 30" 30
                  (lambda () (chan-try-recv! sig #f))
                ) ;
              ) ;
             ) ;
    (check code => -2)
    (check-true (string-contains? text "partial"))
    (check-true (< (- (current-second) t0) 10))
  ) ;let-values
) ;let*

(check-report)
