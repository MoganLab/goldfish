(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; chan-recv!
;; 从通道接收一个值。
;;
;; 语法
;; ----
;; (chan-recv! ch)
;; (chan-recv! ch timeout-ms)
;; (chan-recv! ch timeout-ms default)
;;
;; 参数
;; ----
;; ch : channel
;; 源通道。
;;
;; timeout-ms : integer?
;; 可选。超时毫秒数。缺省时无限等待（Go 语义）。
;;
;; default : any
;; 可选。超时时返回的默认值，缺省为符号 timeout。
;;
;; 返回值
;; ----
;; any
;; 通道中的下一个值；超时返回 default；通道已关闭且读空时返回 eof-object。
;;
;; 错误处理
;; ----
;; ch 不是通道时抛出 type-error。

(define ch (make-chan 2))
(chan-send! ch 1)
(chan-send! ch 2)
(check (chan-recv! ch) => 1)
(check (chan-recv! ch) => 2)

;; 超时返回默认值
(check (chan-recv! ch 50 'empty) => 'empty)
(check (chan-recv! ch 50) => 'timeout)

;; 关闭后读空返回 eof-object
(define ch2 (make-chan 1))
(chan-send! ch2 'x)
(chan-close! ch2)
(check (chan-recv! ch2) => 'x)
(check (eof-object? (chan-recv! ch2)) => #t)

(check-catch 'type-error (chan-recv! "not-a-chan"))

(check-report)
