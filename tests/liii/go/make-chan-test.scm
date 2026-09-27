(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; make-chan
;; 创建一个新通道（channel）。
;;
;; 语法
;; ----
;; (make-chan)
;; (make-chan capacity)
;;
;; 参数
;; ----
;; capacity : integer?
;; 可选。缓冲区容量，默认为 0。
;;
;; 返回值
;; ----
;; channel
;; 新创建的通道对象。
;;
;; 说明
;; ----
;; 1. capacity 为 0 时是无缓冲通道：send 与 recv 必须相遇（rendezvous）才完成。
;; 2. capacity 大于 0 时是有缓冲通道：缓冲未满时 send 立即返回，缓冲满时阻塞。
;;
;; 错误处理
;; ----
;; capacity 为负数或非整数时抛出 type-error。

(define ch1 (make-chan))
(check (chan? ch1) => #t)

(define ch2 (make-chan 20))
(check (chan? ch2) => #t)

(check-catch 'type-error (make-chan -1))
(check-catch 'type-error (make-chan "bad"))

(check-report)
