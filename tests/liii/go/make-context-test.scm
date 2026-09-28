(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; make-context
;; 创建一个可取消的上下文（context），用于跨协程传递取消信号。
;;
;; 语法
;; ----
;; (make-context)
;;
;; 返回值
;; ----
;; context
;; 新的上下文对象。
;;
;; 说明
;; ----
;; context 是对一个可关闭 channel 的封装：context-cancel! 关闭该 channel，
;; 等待方通过 (chan-recv! (context-channel ctx)) 或 select 监听取消信号。

(define ctx (make-context))
(check (context? ctx) => #t)
(check (context-done? ctx) => #f)

(check-report)
