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
;;   这里模拟一个“不响应中止信号”的 worker：它只在超时后自行结束。
;;
;; 断言：
;;   1. 退出流程触发强制退出钩子（宽限期后仍未结算）
;;   2. 主循环在宽限期附近返回（没有等 worker 结束）

;; 模拟在途 LLM 请求：阻塞在 recv 上，直到超时才返回（不发送 agent_settled）
(define (stuck-worker ch)
  (chan-recv! ch 1500)
  #t
) ;define

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
       (forced #f)
       (start (current-monotonic-millis))
      ) ;
  (pi-queue-set-busy! q)
  (go (stuck-worker (make-chan 1)))
  ;; 输入框为空 → Ctrl+D 走 EOF 语义退出
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app "" '() (lambda () (set! forced #t))) => #t)
  ;; 宽限期后仍未结算 → 强制退出钩子被调用
  (check-true forced)
  ;; 未等待 worker 结束（worker 需 1.5s 才返回）
  (check-true (< (- (current-monotonic-millis) start) 1200))
  ;; 队列仍为 busy（worker 从未结算）
  (check (pi-queue-busy? q) => #t)
) ;let*

(check-report)
