;; pi-tui-run-worker
;; 运行单次异步 Worker 会话流程并将流式事件投递至通道。
;;
;; 语法
;; ----
;; (pi-tui-run-worker parsed cfg user-text event-chan . opts)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行参数。
;; cfg : list?
;; 配置列表。
;; user-text : string?
;; 用户输入消息。
;; event-chan : chan?
;; 事件通道。
;; opts : list?
;; 可选参数，首项为复用的会话文件。
;;
;; 返回值
;; ----
;; any
;; 执行流程结果或捕获异常并投递 error 事件。

(import (scheme base)
  (liii check)
  (liii string)
  (liii go)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. 缺失凭证时投递包含 /login 的 error 事件并结束当前轮次
(let ((ev-ch (make-chan 4)))
  (pi-tui-run-worker '() '((provider . "deepseek") (api-key . #f)) "test" ev-ch)
  (let ((ev1 (chan-try-recv! ev-ch #f))
        (ev2 (chan-try-recv! ev-ch #f))
       ) ;
    (check-true (pair? ev1))
    (check (car ev1) => 'error)
    (check-true (string-contains? (cadr ev1) "/login"))
    (check ev2 => '(agent_settled #f))
  ) ;let
) ;let

(check-report)
