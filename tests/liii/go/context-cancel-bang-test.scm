(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; context-cancel!
;; 主动取消一个 context，唤醒所有等待取消信号的协程。
;;
;; 语法
;; ----
;; (context-cancel! ctx)
;;
;; 参数
;; ----
;; ctx : context
;; 要取消的上下文。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 取消是幂等的，重复调用安全。
;; 2. 与 select 配合可实现协作式任务取消。

(define ctx (make-context))
(context-cancel! ctx)
(check (context-done? ctx) => #t)
(context-cancel! ctx)
(check (context-done? ctx) => #t)

;; 与 go + select 配合的协作式取消

(define (cancellable-worker ctx status-ch)
  (let loop
    ()
    (select ((chan-recv! (context-channel ctx) _) (chan-send! status-ch "stopped"))
            (timeout 10 (loop))
    ) ;select
  ) ;let
) ;define

(define ctx-worker (make-context))

(define ch-status (make-chan 1))
(go (cancellable-worker ctx-worker ch-status))
(context-cancel! ctx-worker)
(check (chan-recv! ch-status 2000) => "stopped")

(check-report)
