;; %bash-worker-run-job
;; bash worker 的单任务执行与结算：中止返回 -2 并追加 Command aborted。
;;
;; 语法
;; ----
;; (%bash-worker-run-job id command excluded? abort-chan event-chan)
;;
;; 参数
;; ----
;; id : integer?
;; 任务标识（卡片回填用）。
;; command : string
;; 要执行的 bash 命令。
;; excluded? : boolean?
;; !! 前缀命令不注入会话上下文。
;; abort-chan : channel
;; ESC 中止信号通道（流式轮询点探测）。
;; event-chan : channel
;; bash_complete 事件回传通道。
;;
;; 返回值
;; ----
;; unspecified
;; 结果经 event-chan 以 bash_complete 事件回传。

(import (scheme base)
  (liii check)
  (liii go)
  (liii string)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常执行：bash_complete 事件携带输出与退出码 0
(let* ((ev-ch (make-chan 4))
       (abort-ch (make-chan 1))
       (_ (%bash-worker-run-job 1 "echo hello" #f abort-ch ev-ch))
       (ev (chan-recv! ev-ch 10000 #f))
      ) ;
  (check-true (pair? ev))
  (check (car ev) => 'bash_complete)
  (check (cadr ev) => 1)
  (check-true (string-contains? (cadddr ev) "hello"))
  (check (list-ref ev 4) => 0)
) ;let*

;; 2. 异常命令（语法错误）：bash error 事件，退出码 1
(let* ((ev-ch (make-chan 4))
       (abort-ch (make-chan 1))
       (_ (%bash-worker-run-job 2 "this-is-not-a-command-xyz" #f abort-ch ev-ch))
       (ev (chan-recv! ev-ch 10000 #f))
      ) ;
  (check-true (pair? ev))
  (check (car ev) => 'bash_complete)
  (check (list-ref ev 4) => 127)
) ;let*

(check-report)
