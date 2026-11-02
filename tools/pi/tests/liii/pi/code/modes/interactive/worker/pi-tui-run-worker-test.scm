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
  (liii go)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. 运行过程异常时安全捕获并投递 error 事件
(let ((ev-ch (make-chan 4)))
  ;; 传入空配置使流程异常，断言异常被安全拦截入通道
  (pi-tui-run-worker '() '() "test" ev-ch)
  (let ((ev (chan-try-recv! ev-ch #f)))
    (check-true (pair? ev))
    (check (car ev) => 'error)
  ) ;let
) ;let

(check-report)
