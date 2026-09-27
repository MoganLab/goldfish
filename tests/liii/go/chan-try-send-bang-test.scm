(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; chan-try-send!
;; 非阻塞地向通道发送一个值。
;;
;; 语法
;; ----
;; (chan-try-send! ch val)
;;
;; 参数
;; ----
;; ch : channel
;; 目标通道。
;;
;; val : any
;; 要发送的值（序列化规则同 chan-send!）。
;;
;; 返回值
;; ----
;; boolean
;; 缓冲未满（或无缓冲通道有等待的接收者）时发送成功返回 #t；
;; 无法立即完成时返回 #f。

(define ch (make-chan 1))
(check (chan-try-send! ch 'a) => #t)
(check (chan-try-send! ch 'b) => #f)
(check (chan-recv! ch) => 'a)
(check (chan-try-send! ch 'b) => #t)

(check-report)
