(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; chan-close!
;; 关闭通道。
;;
;; 语法
;; ----
;; (chan-close! ch)
;;
;; 参数
;; ----
;; ch : channel
;; 要关闭的通道。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 关闭后通道中已有的数据仍可正常读出，读空后 recv 返回 eof-object。
;; 2. 关闭会唤醒所有阻塞中的发送者和接收者。
;; 3. 重复关闭是安全的（幂等）。
;;
;; 错误处理
;; ----
;; ch 不是通道时抛出 type-error。关闭后发送抛出 value-error。

(define ch (make-chan 5))
(chan-send! ch 100)
(chan-send! ch 200)
(chan-close! ch)

(check (chan-recv! ch) => 100)
(check (chan-recv! ch) => 200)
(check (eof-object? (chan-recv! ch)) => #t)

(chan-close! ch)
(check-catch 'value-error (chan-send! ch 300))
(check-catch 'type-error (chan-close! "not-a-chan"))

(check-report)
