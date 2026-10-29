(import (scheme base)
  (liii check)
  (liii go)
  (liii string)
  (liii time)
  (liii date-time)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report)

;; working 状态（队列 busy）下按 Ctrl+D 退出：
;;   不得因后台 worker（在途 LLM 流式请求）阻塞而卡住——
;;   进程退出会 join go 线程池，worker 不结束就永远拿不回 shell 提示符。
;;   这里模拟一个“不响应中止信号”的 worker：它一直阻塞在 recv 上，
;;   直到测试结束关闭通道才返回（不发送 agent_settled）。
;;
;; 断言：
;;   1. 退出流程触发强制退出钩子（宽限期后仍未结算 → 跳过线程池 join）
;;   2. 队列仍为 busy（worker 从未结算）
;;
;; 注：不在此断言退出耗时——pi-app-run 内含分支监听等与本用例无关的初始化，
;; 其耗时随平台差异大（Windows CI 明显更慢），耗时断言放在
;; pi-app-abort-inflight-test.scm 中对纯中止等待做隔离测量。

;; 模拟在途 LLM 请求：阻塞在 recv 上，直到通道关闭才返回
(define (stuck-worker ch)
  (chan-recv! ch 10000)
  #t
) ;define

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (worker-chan (make-chan 1))
       (forced #f)
      ) ;
  (pi-queue-set-busy! q)
  (go (stuck-worker worker-chan))
  ;; 输入框为空 → Ctrl+D 走 EOF 语义退出
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app "" '() (lambda () (set! forced #t))) => #t)
  ;; 宽限期后仍未结算 → 强制退出钩子被调用
  (check-true forced)
  ;; 队列仍为 busy（worker 从未结算）
  (check (pi-queue-busy? q) => #t)
  ;; 释放 worker：关闭通道令阻塞的 recv 返回，避免进程退出时 join 长时间等待
  (chan-close! worker-chan)
) ;let*

(check-report)
