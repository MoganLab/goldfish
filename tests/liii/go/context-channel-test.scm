(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; context-channel
;; 返回 context 内部的完成通知 channel。
;;
;; 语法
;; ----
;; (context-channel ctx)
;;
;; 参数
;; ----
;; ctx : context
;; 目标上下文。
;;
;; 返回值
;; ----
;; channel
;; context 完成（取消/超时）时会被关闭的通道。
;;
;; 说明
;; ----
;; 对该通道 recv：context 未完成时阻塞，完成后返回 eof-object。
;; 可直接用于 select 的取消分支。

(define ctx (make-context))
(define ch (context-channel ctx))
(check (chan? ch) => #t)
(check (chan-closed? ch) => #f)
(context-cancel! ctx)
(check (eof-object? (chan-recv! ch 100)) => #t)

(check-report)
